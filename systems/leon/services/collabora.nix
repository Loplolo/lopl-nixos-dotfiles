{
  config,
  lib,
  pkgs,
  ...
}: let
  port = 9980;
  host = "office.lopl.dev";
  cloud = config.services.nextcloud.hostName;
  occ = lib.getExe config.services.nextcloud.occ;
in {
  services.collabora-online = {
    enable = true;
    inherit port;
    settings = {
      server_name = host;
      ssl = {
        enable = false;
        termination = true;
      };
      net = {
        listen = "loopback";
        post_allow.host = ["::1" "127.0.0.1"];
      };
      storage.wopi = {
        "@allow" = true;
        host = [cloud];
      };
    };
  };

  # Collabora fetches documents from Nextcloud through the local Caddy.
  networking.hosts."127.0.0.1" = [cloud];

  # Point Nextcloud Office at Collabora. Runs in the background and retries,
  # since coolwsd takes a while to answer after starting.
  systemd.services.nextcloud-collabora-config = {
    wantedBy = ["multi-user.target"];
    after = ["nextcloud-setup.service" "coolwsd.service"];
    requires = ["coolwsd.service"];
    serviceConfig = {
      Type = "simple";
      Restart = "on-failure";
      RestartSec = 30;
    };
    path = [pkgs.curl];
    script = ''
      for _ in $(seq 90); do
        curl -fso /dev/null http://127.0.0.1:${toString port}/hosting/discovery && break
        sleep 2
      done
      ${occ} config:app:set richdocuments wopi_url --value http://127.0.0.1:${toString port}
      ${occ} config:app:set richdocuments public_wopi_url --value https://${host}
      ${occ} config:app:set richdocuments wopi_allowlist --value 127.0.0.1,::1
      ${occ} richdocuments:activate-config
    '';
  };

  lopl.proxies.office = port;
}
