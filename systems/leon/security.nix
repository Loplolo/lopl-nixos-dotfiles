{
  config,
  lib,
  pkgs,
  ...
}: let
  tailnet = ["100.64.0.0/10" "fd7a:115c:a1e0::/48"];

  security-alert = import ./alert.nix {inherit config pkgs;};
  sandbox = import ./sandbox.nix;

  audit-watch = pkgs.writeShellApplication {
    name = "audit-watch";
    runtimeInputs = with pkgs; [coreutils systemd security-alert];
    text = ''
      state="''${RUNTIME_DIRECTORY:-/run/audit-watch}"
      mkdir -p "$state"

      journalctl -f -n 0 -o cat -t audisp-syslog -t sudo | while IFS= read -r line; do
        case "$line" in
          *'key=identity'* | *'key="identity"'*)
            key=identity
            label="account change"
            ;;
          *'key=privilege'* | *'key="privilege"'*)
            key=privilege
            label="sudoers change"
            ;;
          *'key=sshd'* | *'key="sshd"'*)
            key=sshd
            label="SSH key/config change"
            ;;
          *'key=persistence'* | *'key="persistence"'*)
            key=persistence
            label="persistence change"
            ;;
          *'key=exec_tmp'* | *'key="exec_tmp"'*)
            key=exec_tmp
            label="process executed from tmp"
            ;;
          *"incorrect password attempt"* | *"NOT in sudoers"* | *"command not allowed"* | *"user NOT authorized"*)
            key=sudo
            label="sudo failure"
            ;;
          *) continue ;;
        esac

        now=$(date +%s)
        last=0
        if [ -f "$state/$key" ]; then last=$(cat "$state/$key"); fi

        cooldown=30
        case "$key" in
          exec_tmp) cooldown=300 ;;
        esac
        if [ $((now - last)) -lt $cooldown ]; then continue; fi
        printf '%s' "$now" >"$state/$key"

        security-alert "$(printf '%s: %s' "$label" "$line" | head -c 3500)"
      done
    '';
  };
in {
  sops.secrets = {
    tg-homelab-bot-token = {};
    security-alert-chat-id = {};
  };
  sops.templates."security-alert-env" = {
    mode = "0400";
    content = ''
      TG_TOKEN=${config.sops.placeholder."tg-homelab-bot-token"}
      TG_CHAT=${config.sops.placeholder."security-alert-chat-id"}
    '';
  };

  environment.systemPackages = [security-alert pkgs.lynis pkgs.audit];

  services.openssh.openFirewall = false;
  services.openssh.settings.AllowUsers = ["lopl" config.services.forgejo.user];

  # Kernel-level auditing replaces the old snapshot/diff watchers.
  security.auditd = {
    enable = true;
    settings = {
      max_log_file = 50;
      num_logs = 5;
      max_log_file_action = "ROTATE";
    };
    plugins.syslog = {
      active = true;
      args = ["interpret"];
    };
  };
  security.audit = {
    enable = true;
    backlogLimit = 8192;
    rules = [
      "-w /etc/passwd -p wa -k identity"
      "-w /etc/shadow -p wa -k identity"
      "-w /etc/group -p wa -k identity"
      "-w /etc/sudoers -p wa -k privilege"
      "-w /etc/ssh -p wa -k sshd"
      "-w /etc/ssh/authorized_keys.d -p wa -k sshd"
      "-w /etc/pam.d -p wa -k persistence"
      "-w /run/systemd/system -p wa -k persistence"
      "-a always,exit -F arch=b64 -F dir=/tmp -F perm=x -F auid>=1000 -F auid!=unset -k exec_tmp"
      "-a always,exit -F arch=b64 -F dir=/var/tmp -F perm=x -F auid>=1000 -F auid!=unset -k exec_tmp"
      "-a always,exit -F arch=b64 -F dir=/dev/shm -F perm=x -F auid>=1000 -F auid!=unset -k exec_tmp"
    ];
  };

  systemd.services.audit-watch = {
    description = "Forward audit and sudo events to Telegram";
    wantedBy = ["multi-user.target"];
    wants = ["auditd.service"];
    after = ["auditd.service"];
    serviceConfig =
      sandbox
      // {
        ExecStart = lib.getExe audit-watch;
        Restart = "always";
        RestartSec = 10;
        RuntimeDirectory = "audit-watch";
      };
  };

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
          action = "security-alert[jail=%(__name__)s, msg=\"ssh login from\"]";
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
    actionban = ${lib.getExe security-alert} "<msg> <ip> (<jail>, <failures>)"
    actionunban =

    [Init]
    jail = fail2ban
    msg = banned
  '';
}
