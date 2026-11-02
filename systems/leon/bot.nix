{
  config,
  lib,
  pkgs,
  ...
}: let
  alert = import ./alert.nix {inherit config pkgs;};
  sandbox = import ./sandbox.nix;
  alertEnv = config.sops.templates."security-alert-env".path;

  watched =
    [
      "caddy"
      "nginx"
      "phpfpm-nextcloud"
      "postgresql"
      "redis-nextcloud"
      "coolwsd"
      "forgejo"
      "immich-server"
      "immich-machine-learning"
      "redis-immich"
      "navidrome"
      "adguardhome"
      "syncthing"
      "podman-homeassistant"
      "podman-shieldy-bot"
      "podman-shieldy-mongo"
      "podman-interpelli-bot"
      "tailscaled"
      "sshd"
      "status-bot"
    ]
    ++ lib.filter (lib.hasPrefix "cloudflared-tunnel-") (lib.attrNames config.systemd.services);

  status-report = pkgs.writeShellApplication {
    name = "status-report";
    runtimeInputs = with pkgs; [
      coreutils
      gawk
      gnugrep
      gnused
      procps
      util-linux
      systemd
    ];
    text = ''
      secs=$(cut -d. -f1 /proc/uptime)
      echo "${config.networking.hostName} up $((secs / 86400))d $((secs % 86400 / 3600))h, load $(cut -d' ' -f1-3 /proc/loadavg)"
      echo "mem $(free -h | awk '/^Mem:/ {print $3 "/" $2}' | sed 's/i//g')"
      for m in / /mnt/storage; do
        if mountpoint -q "$m"; then
          df -h --output=used,size,pcent "$m" | tail -n 1 | awk -v m="$m" '{print m " " $1 "/" $2 " " $3}'
        else
          echo "$m not mounted"
        fi
      done

      down=""
      for u in ${lib.concatStringsSep " " watched}; do
        s=$(systemctl is-active "$u.service" || true)
        [ "$s" = active ] || down="$down $u($s)"
      done
      if [ -z "$down" ]; then echo "services ok"; else echo "down:$down"; fi
      failed=$(systemctl --failed --no-legend --plain | awk '{print $1}' | tr '\n' ' ')
      [ -z "$failed" ] || echo "failed: $failed"

      last=$(journalctl -q -u restic-backups-nextcloud.service -o short-iso -n 1 \
        --grep 'Finished|Failed with result' 2>/dev/null || true)
      case "$last" in
        "") echo "backup never" ;;
        *Failed*) echo "backup FAILED ''${last:0:16}" ;;
        *) echo "backup ok ''${last:0:16}" | tr T ' ' ;;
      esac

      echo "built $(date -d "@$(stat -c %Y /nix/var/nix/profiles/system)" '+%Y-%m-%d %H:%M')"
      if [ "$(readlink /run/booted-system/kernel)" != "$(readlink /run/current-system/kernel)" ]; then
        echo "reboot pending (new kernel)"
      fi
    '';
  };

  health-check = pkgs.writeShellApplication {
    name = "health-check";
    runtimeInputs = with pkgs; [coreutils gawk gnugrep gnused systemd iproute2 alert];
    text = ''
      export LC_ALL=C
      cd "$STATE_DIRECTORY"

      {
        for u in ${lib.concatStringsSep " " watched}; do
          s=$(systemctl is-active "$u.service" || true)
          [ "$s" = active ] || echo "$u is $s"
        done
        systemctl --failed --no-legend --plain | awk '{print $1 " failed"}'
        for m in / /var/lib /mnt/storage; do
          if mountpoint -q "$m"; then
            df --output=pcent "$m" | tail -n 1 | awk -v m="$m" '$1 + 0 >= 90 {print m " is " $1 " full"}'
          fi
        done
        awk '/^MemTotal:/ {t = $2} /^MemAvailable:/ {a = $2} END {if (a / t < 0.1) print "memory below 10%"}' /proc/meminfo
        awk -v n="$(nproc)" '$2 > 2 * n {print "load above " 2 * n}' /proc/loadavg
        last=$(systemctl show --timestamp=unix -P LastTriggerUSec restic-backups-nextcloud.timer | tr -d @)
        case "$last" in "" | *[!0-9]*) last=0 ;; esac
        [ $(($(date +%s) - last)) -lt $((30 * 3600)) ] || echo "backup not run in 30h"
      } | sort -u >problems.new

      ss -Hltn | awk '{print $4}' | grep -vE '^(127\.|\[::1\]|\[::ffff:127\.|100\.|\[fd7a:)' |
        awk -F: '$NF < 32768 {print $NF}' | sort -u >ports.new

      touch problems
      msg=$(
        comm -13 problems problems.new | sed 's/^/problem: /'
        comm -23 problems problems.new | sed 's/^/resolved: /'
        if [ -f ports ]; then
          comm -13 ports ports.new | sed 's/^/new listening port: /'
        fi
      )
      mv problems.new problems
      mv ports.new ports
      [ -z "$msg" ] || security-alert "$msg"
    '';
  };

  status-bot = pkgs.writeShellApplication {
    name = "status-bot";
    runtimeInputs = with pkgs; [coreutils curl jq status-report alert];
    excludeShellChecks = ["SC1091"];
    text = ''
      . ${alertEnv}
      state="$STATE_DIRECTORY/offset"
      offset=$(cat "$state" 2>/dev/null || echo 0)

      api() {
        printf 'url = "https://api.telegram.org/bot%s/%s"\n' "$TG_TOKEN" "$1" |
          curl -fsS --max-time 70 -K - "''${@:2}"
      }

      while true; do
        if ! resp=$(api getUpdates --data-urlencode "offset=$offset" \
          --data-urlencode "timeout=55" --data-urlencode 'allowed_updates=["message"]'); then
          sleep 15
          continue
        fi
        while IFS=$'\t' read -r id chat text; do
          offset=$((id + 1))
          echo "$offset" >"$state"
          [ "$chat" = "$TG_CHAT" ] || continue
          case "$text" in
            /status*) status-report 2>&1 | security-alert ;;
            /start* | /help*) security-alert "/status" ;;
          esac
        done < <(printf '%s' "$resp" | jq -r '.result[] |
          [.update_id, (.message.chat.id // "" | tostring), (.message.text // "" | gsub("[\t\n]"; " "))] | @tsv')
      done
    '';
  };
in {
  environment.systemPackages = [status-report];

  systemd.services.health-check = {
    description = "Alert on service, resource and listening port changes";
    startAt = "*:0/5";
    serviceConfig =
      sandbox
      // {
        Type = "oneshot";
        ExecStart = lib.getExe health-check;
        StateDirectory = "health-check";
        RestrictAddressFamilies = ["AF_UNIX" "AF_INET" "AF_INET6" "AF_NETLINK"];
      };
  };

  systemd.services.status-bot = {
    description = "Telegram status bot";
    wantedBy = ["multi-user.target"];
    wants = ["network-online.target"];
    after = ["network-online.target"];
    serviceConfig =
      sandbox
      // {
        ExecStart = lib.getExe status-bot;
        Restart = "always";
        RestartSec = 30;
        StateDirectory = "status-bot";
      };
  };
}
