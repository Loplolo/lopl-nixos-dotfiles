{
  config,
  pkgs,
  ...
}: {
  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud35;
    hostName = "cloud.lopl.dev";

    config.dbtype = "pgsql";
    database.createLocally = true;
    https = true;

    settings = {
      trusted_domains = ["cloud.lopl.dev"];
      trusted_proxies = ["127.0.0.1" "::1"];
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
        groupfolders
        richdocuments
        ;

      passwords = pkgs.fetchNextcloudApp {
        url = "https://git.mdns.eu/api/v4/projects/45/packages/generic/passwords/2026.10.0/passwords.tar.gz";
        hash = "sha256-EXxP3Y0m+i6RIUJ2qVENQ6m+NE/+RMbXQbnEjCsQTSU=";
        license = "agpl3Plus";
      };
    };
    extraAppsEnable = true;
  };

  # Keep uploads in chunks below Cloudflare's 100 MB request limit.
  systemd.services.nextcloud-chunk-size = {
    wantedBy = ["multi-user.target"];
    after = ["nextcloud-setup.service"];
    serviceConfig.Type = "oneshot";
    script = ''
      ${pkgs.lib.getExe config.services.nextcloud.occ} config:app:set files max_chunk_size --value 52428800
    '';
  };

  services.nginx = {
    enable = true;
    virtualHosts."cloud.lopl.dev" = {
      listen = [
        {
          addr = "127.0.0.1";
          port = config.lopl.proxies.cloud;
        }
      ];
      addSSL = false;
      forceSSL = false;
      useACMEHost = null;
    };
  };

  users.groups = {
    media.members = ["nextcloud" "navidrome" "immich" "lopl"];
    syncthing.members = ["nextcloud" "lopl"];
  };

  systemd.tmpfiles.rules = [
    "d /mnt/media/music 0775 root media -"
    "d /mnt/media/photos 0775 root media -"
    "d /var/lib/syncthing 0750 syncthing syncthing -"
  ];

  lopl.proxies.cloud = 8081;
}
