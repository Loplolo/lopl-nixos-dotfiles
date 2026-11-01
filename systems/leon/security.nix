{
  config,
  lib,
  pkgs,
  ...
}: let
  tailnet = ["100.64.0.0/10" "fd7a:115c:a1e0::/48"];

  alertEnv = config.sops.templates."security-alert-env".path;

  security-alert = pkgs.writeShellApplication {
    name = "security-alert";
    runtimeInputs = with pkgs; [curl util-linux coreutils];
    excludeShellChecks = ["SC1091"];
    text = ''
      if [ "$#" -gt 0 ]; then msg="$*"; else msg=$(cat); fi
      msg="[${config.networking.hostName}] $msg"
      printf '%s\n' "$msg" | logger -t security-alert

      . ${alertEnv}
      printf 'url = "https://api.telegram.org/bot%s/sendMessage"\n' "$TG_TOKEN" |
        curl -fsS --max-time 20 -K - \
          --data-urlencode "chat_id=$TG_CHAT" \
          --data-urlencode "text=$(printf '%s' "$msg" | head -c 4000)" \
          -o /dev/null ||
        logger -t security-alert "Telegram delivery failed"
    '';
  };

  security-digest = pkgs.writeShellApplication {
    name = "security-digest";
    runtimeInputs = with pkgs; [
      coreutils
      gnugrep
      gnused
      gawk
      findutils
      diffutils
      iproute2
      systemd
      config.services.fail2ban.package
      security-alert
    ];
    text = ''
      state=/var/lib/security-digest
      since="-24h"
      out=$(mktemp)
      trap 'rm -f "$out"' EXIT
      say() { printf '%s\n' "$*" >>"$out"; }
      count() { grep -cE "$1" || true; }

      say "Security digest, last 24h"

      ssh_log=$(journalctl -q -o cat -u sshd.service --since "$since" || true)
      say ""
      say "SSH logins accepted: $(printf '%s\n' "$ssh_log" | count '^Accepted ')"
      printf '%s\n' "$ssh_log" | grep -E '^Accepted ' |
        awk '{print "  " $4 " from " $6 " (" $2 ")"}' | sort | uniq -c | head -n 10 >>"$out" || true
      say "SSH failed attempts: $(printf '%s\n' "$ssh_log" | count 'Failed |Invalid user|authentication failure')"

      sudo_log=$(journalctl -q -o cat _COMM=sudo --since "$since" || true)
      say "sudo commands: $(printf '%s\n' "$sudo_log" | count 'COMMAND=')"
      say "sudo failures: $(printf '%s\n' "$sudo_log" | count 'incorrect password|authentication failure|NOT in sudoers')"

      nc_fail=$(journalctl -q -o cat SYSLOG_IDENTIFIER=Nextcloud --since "$since" | count 'Login failed')
      fj_fail=$(journalctl -q -o cat -u forgejo.service --since "$since" | count 'Failed authentication attempt')
      say "Nextcloud failed logins: $nc_fail"
      say "Forgejo failed logins: $fj_fail"

      say ""
      say "fail2ban bans: $(journalctl -q -o cat -u fail2ban.service --since "$since" | count '\] Ban ')"
      for jail in $(fail2ban-client status 2>/dev/null | sed -n 's/.*Jail list:\s*//p' | tr ',' ' '); do
        n=$(fail2ban-client status "$jail" | sed -n 's/.*Currently banned:\s*//p')
        [ "''${n:-0}" != 0 ] && say "  $jail: $n banned now"
      done

      fw=$(journalctl -q -k -o cat --since "$since" | grep 'refused connection' || true)
      say "Firewall refused connections: $(printf '%s' "$fw" | count 'refused')"
      printf '%s\n' "$fw" | grep -oE 'SRC=[^ ]+' | sort | uniq -c | sort -rn | head -n 5 |
        awk '{print "  " $1 "x " substr($2, 5)}' >>"$out" || true

      failed=$(systemctl --failed --no-legend --plain | awk '{print $1}')
      if [ -n "$failed" ]; then
        say ""
        say "Failed units:"
        printf '%s\n' "$failed" | sed 's/^/  /' >>"$out"
      fi

      changes=""
      check() {
        local name=$1 now="$state/$1.now"
        shift
        "$@" >"$now" 2>/dev/null || true
        if [ -f "$state/$name.prev" ]; then
          d=$(diff "$state/$name.prev" "$now" | grep -E '^[<>]' | head -n 15 || true)
          [ -n "$d" ] && changes="$changes"$'\n'"$name changed:"$'\n'"$d"
        fi
        mv "$now" "$state/$name.prev"
      }
      check listening-ports sh -c "ss -tulnH | awk '{print \$1, \$5}' | sort -u"
      check ssh-keys sh -c 'sha256sum /root/.ssh/authorized_keys /home/*/.ssh/authorized_keys \
        /var/lib/forgejo/.ssh/authorized_keys /etc/ssh/authorized_keys.d/* 2>/dev/null | sort -k2'
      check suid-files sh -c 'find / -xdev \( -path /nix -o -path /var/lib/containers -o -path /proc \) -prune \
        -o -type f -perm /6000 -print 2>/dev/null | sort'
      check users sh -c "getent passwd | awk -F: '\$3 == 0 || \$7 !~ /nologin|false/ {print \$1, \$3, \$7}' | sort"

      say ""
      if [ -n "$changes" ]; then say "CHANGES SINCE LAST DIGEST:$changes"; else say "No changes in ports, SSH keys, SUID files or login users."; fi

      cat "$out"
      security-alert <"$out"
    '';
  };
in {
  sops.secrets = {
    security-alert-bot-token = {};
    security-alert-chat-id = {};
  };
  sops.templates."security-alert-env" = {
    mode = "0400";
    content = ''
      TG_TOKEN=${config.sops.placeholder."security-alert-bot-token"}
      TG_CHAT=${config.sops.placeholder."security-alert-chat-id"}
    '';
  };

  environment.systemPackages = [security-alert security-digest pkgs.lynis];

  services.openssh.openFirewall = false;
  services.openssh.settings.AllowUsers = ["lopl" config.services.forgejo.user];

  services.fail2ban = {
    enable = true;
    maxretry = 5;
    bantime = "1h";
    ignoreIP = tailnet;
    bantime-increment = {
      enable = true;
      maxtime = "1w";
    };

    jails = {
      DEFAULT.settings.action = "%(action_)s\n         security-alert[jail=%(__name__)s]";

      sshd.settings.mode = "aggressive";

      nextcloud = {
        settings = {
          port = "http,https";
          findtime = "15m";
        };
        filter.Definition = {
          failregex = ''
            ^.*Login failed: .* \(Remote IP: '?<HOST>'?\)
                        ^.*"remoteAddr":"<HOST>".*Trusted domain error'';
          journalmatch = "SYSLOG_IDENTIFIER=Nextcloud";
        };
      };

      forgejo = {
        settings = {
          port = "http,https,ssh";
          findtime = "15m";
        };
        filter.Definition = {
          failregex = "^.*Failed authentication attempt for .* from <HOST>(?::\\d+)?:";
          journalmatch = "_SYSTEMD_UNIT=forgejo.service";
        };
      };

      homeassistant = {
        settings = {
          port = "8123,http,https";
          findtime = "15m";
        };
        filter.Definition = {
          failregex = "^.*Login attempt or request with invalid authentication from <HOST>";
          journalmatch = "CONTAINER_NAME=homeassistant";
        };
      };

      ssh-login = {
        settings = {
          maxretry = 1;
          findtime = "1m";
          bantime = "1m";
          action = "security-alert[jail=%(__name__)s, msg=\"SSH login accepted from\"]";
        };
        filter.Definition = {
          failregex = "^Accepted \\S+ for \\S+ from <HOST>";
          journalmatch = "_SYSTEMD_UNIT=sshd.service";
        };
      };
    };
  };

  environment.etc."fail2ban/action.d/security-alert.conf".text = ''
    [Definition]
    actionstart =
    actionstop =
    actioncheck =
    actionban = ${lib.getExe security-alert} "<msg> <ip> (<jail>, <failures> hits)"
    actionunban =

    [Init]
    jail = fail2ban
    msg = fail2ban banned
  '';

  systemd.services.security-digest = {
    description = "Daily security digest";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = lib.getExe security-digest;
      StateDirectory = "security-digest";
      StateDirectoryMode = "0700";
    };
  };
  systemd.timers.security-digest = {
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "08:00";
      Persistent = true;
    };
  };
}
