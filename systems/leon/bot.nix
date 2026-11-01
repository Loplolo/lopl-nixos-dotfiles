{
  config,
  lib,
  pkgs,
  ...
}: let
  alert = import ./alert.nix {inherit config pkgs;};
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
      "tailscaled"
      "fail2ban"
      "sshd"
    ]
    ++ lib.filter (lib.hasPrefix "cloudflared-tunnel-") (lib.attrNames config.systemd.services);

  status-report = pkgs.writeShellApplication {
    name = "status-report";
    runtimeInputs = with pkgs; [
      coreutils
      gawk
      gnugrep
      procps
      util-linux
      systemd
      config.services.fail2ban.package
    ];
    text = ''
      secs=$(cut -d. -f1 /proc/uptime)
      echo "$(hostname) up $((secs / 86400))d $((secs % 86400 / 3600))h, load $(cut -d' ' -f1-3 /proc/loadavg)"
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

      banned=0
      for jail in $(fail2ban-client status 2>/dev/null | sed -n 's/.*Jail list:\s*//p' | tr ',' ' '); do
        n=$(fail2ban-client status "$jail" | sed -n 's/.*Currently banned:\s*//p')
        banned=$((banned + ''${n:-0}))
      done
      echo "bans $banned"

      echo "built $(date -d "@$(stat -c %Y /nix/var/nix/profiles/system)" '+%Y-%m-%d %H:%M')"
      if [ "$(readlink /run/booted-system/kernel)" != "$(readlink /run/current-system/kernel)" ]; then
        echo "reboot pending (new kernel)"
      fi
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

  backup-report = pkgs.writeShellApplication {
    name = "backup-report";
    runtimeInputs = with pkgs; [systemd alert];
    text = ''
      unit=restic-backups-nextcloud.service
      start=$(systemctl show "$unit" -p ExecMainStartTimestampMonotonic --value)
      end=$(systemctl show "$unit" -p ExecMainExitTimestampMonotonic --value)
      security-alert "backup ok ($(((end - start) / 60000000)) min)"
    '';
  };

  notify-failure = pkgs.writeShellApplication {
    name = "notify-failure";
    runtimeInputs = with pkgs; [coreutils systemd alert];
    text = ''
      unit="$1.service"
      stamp="$STATE_DIRECTORY/$1"
      if [ -f "$stamp" ] && [ $(($(date +%s) - $(stat -c %Y "$stamp"))) -lt 900 ]; then
        exit 0
      fi
      touch "$stamp"
      log=$(journalctl -q -u "$unit" -n 15 -o cat --no-pager 2>/dev/null | tail -c 3000 || true)
      security-alert "$(printf '%s failed\n\n%s' "$unit" "$log")"
    '';
  };

  sandbox = {
    NoNewPrivileges = true;
    PrivateTmp = true;
    PrivateDevices = true;
    ProtectSystem = "strict";
    ProtectHome = "read-only";
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    ProtectKernelLogs = true;
    ProtectControlGroups = true;
    ProtectClock = true;
    ProtectHostname = true;
    RestrictNamespaces = true;
    RestrictRealtime = true;
    RestrictSUIDSGID = true;
    LockPersonality = true;
    SystemCallArchitectures = "native";
    RestrictAddressFamilies = ["AF_UNIX" "AF_INET" "AF_INET6"];
    CapabilityBoundingSet = "";
  };
in {
  environment.systemPackages = [status-report];

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

  systemd.services.restic-backups-nextcloud.onSuccess = ["backup-report.service"];
  systemd.services.backup-report = {
    description = "Report a successful backup";
    serviceConfig =
      {
        Type = "oneshot";
        ExecStart = lib.getExe backup-report;
      }
      // sandbox;
  };

  systemd.services."notify-failure@" = {
    description = "Report failure of %i";
    serviceConfig =
      sandbox
      // {
        Type = "oneshot";
        ExecStart = "${lib.getExe notify-failure} %i";
        StateDirectory = "notify-failure";
      };
  };

  systemd.packages = [
    (pkgs.runCommand "notify-on-failure" {} ''
      mkdir -p $out/etc/systemd/system/service.d $out/etc/systemd/system/notify-failure@.service.d
      printf '[Unit]\nOnFailure=notify-failure@%%N.service\n' \
        >$out/etc/systemd/system/service.d/90-notify-failure.conf
      printf '[Unit]\nOnFailure=\n' \
        >$out/etc/systemd/system/notify-failure@.service.d/90-notify-failure.conf
    '')
  ];
}
