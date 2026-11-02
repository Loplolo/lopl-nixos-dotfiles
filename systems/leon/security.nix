{
  config,
  lib,
  pkgs,
  ...
}: let
  security-alert = import ./alert.nix {inherit config pkgs;};
  sandbox = import ./sandbox.nix;

  security-watch = pkgs.writeShellApplication {
    name = "security-watch";
    runtimeInputs = with pkgs; [coreutils systemd security-alert];
    text = ''
      declare -A last

      journalctl -f -n 0 -o cat \
        SYSLOG_IDENTIFIER=sshd SYSLOG_IDENTIFIER=sshd-session \
        SYSLOG_IDENTIFIER=sudo SYSLOG_IDENTIFIER=su \
        SYSLOG_IDENTIFIER=useradd SYSLOG_IDENTIFIER=usermod SYSLOG_IDENTIFIER=userdel \
        SYSLOG_IDENTIFIER=groupadd SYSLOG_IDENTIFIER=groupmod SYSLOG_IDENTIFIER=passwd \
        SYSLOG_IDENTIFIER=chpasswd SYSLOG_IDENTIFIER=Nextcloud \
        + _SYSTEMD_UNIT=tailscaled.service _SYSTEMD_UNIT=forgejo.service \
        + CONTAINER_NAME=homeassistant |
        while IFS= read -r line; do
          cooldown=60
          case "$line" in
            "Accepted "* | *"handling new SSH connection"*) label="ssh login" ;;
            *"incorrect password attempt"* | *"NOT in sudoers"* | *"command not allowed"* | *"authentication failure"*) label="local auth failure" ;;
            *"pam_unix(su"*"session opened"*) label="su session" ;;
            *"new user"* | *"new group"* | *"delete user"* | *"password changed"* | *"add '"*"' to "* | *"change user"*) label="account change" ;;
            *"Login failed"* | *"Bruteforce attempt"* | *"Failed authentication attempt"* | *"invalid authentication from"*)
              label="web login failure"
              cooldown=600
              ;;
            *) continue ;;
          esac

          now=$(date +%s)
          if [ $((now - ''${last[$label]:-0})) -lt "$cooldown" ]; then continue; fi
          last[$label]=$now

          security-alert "$label: $(printf '%s' "$line" | head -c 1000)"
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

  environment.systemPackages = [security-alert];

  services.openssh.openFirewall = false;
  services.openssh.settings.AllowUsers = ["lopl" config.services.forgejo.user];

  systemd.services.security-watch = {
    description = "Forward notable auth events to Telegram";
    wantedBy = ["multi-user.target"];
    serviceConfig =
      sandbox
      // {
        ExecStart = lib.getExe security-watch;
        Restart = "always";
        RestartSec = 10;
      };
  };
}
