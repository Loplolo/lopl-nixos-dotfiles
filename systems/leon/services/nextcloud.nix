{
  config,
  pkgs,
  ...
}: {
  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud33;
    hostName = "cloud.lopl.dev";

    config.dbtype = "pgsql";
    database.createLocally = true;
    config.adminpassFile = config.sops.secrets.nextcloud-admin-pass.path; #
    config.adminuser = "admin";

    https = true;

    settings = {
      trusted_domains = ["cloud.lopl.dev"];
      trusted_proxies = ["127.0.0.1"];
      overwriteprotocol = "https";
    };

    extraApps = {
      inherit
        (config.services.nextcloud.package.packages.apps)
        calendar
        contacts
        tasks
        notes
        deck
        news
        ;
    };
    extraAppsEnable = true;
  };

  services.nginx = {
    enable = true;
    virtualHosts."cloud.lopl.dev" = {
      listen = [
        {
          addr = "127.0.0.1";
          port = 8081;
        }
      ];
      addSSL = false;
      forceSSL = false;
      useACMEHost = null;
    };
  };

  users.groups.media.members = ["nextcloud" "navidrome" "immich" "lopl"];
  users.groups.syncthing.members = ["nextcloud" "lopl"];

  systemd.tmpfiles.rules = [
    "d /mnt/media/music 0775 root media -"
    "d /mnt/media/photos 0775 root media -"
    "d /var/lib/syncthing 0750 syncthing syncthing -"
  ];
}
