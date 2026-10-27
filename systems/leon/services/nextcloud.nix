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
    # /mnt/storage is mounted with gid=100 (the "users" group) and is group-writable.
    users.members = ["nextcloud"];
  };

  systemd.tmpfiles.rules = [
    "d /mnt/media/music 2775 root media -"
    "d /mnt/media/photos 2775 root media -"
    "d /var/lib/syncthing 0750 syncthing syncthing -"
  ];

  systemd.services.phpfpm-nextcloud.serviceConfig.UMask = "0002";
  systemd.services.nextcloud-cron.serviceConfig.UMask = "0002";

  systemd.services.media-permissions = {
    wantedBy = ["multi-user.target"];
    after = ["systemd-tmpfiles-setup.service"];
    before = ["navidrome.service"];
    serviceConfig.Type = "oneshot";
    script = ''
      for d in /mnt/media/music /mnt/media/photos; do
        chgrp -R media "$d"
        chmod -R g+rwX "$d"
        find "$d" -type d -exec chmod g+s {} +
      done
    '';
  };

  lopl.proxies.cloud = 8081;
}
