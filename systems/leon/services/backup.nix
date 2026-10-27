{config, ...}: let
  nc = config.services.nextcloud;
in {
  # Company data lives in Nextcloud group folders; personal files are not backed up here.
  # The DB dump covers the whole instance (shares, passwords app, calendars).
  services.postgresqlBackup = {
    enable = true;
    databases = [nc.config.dbname];
    location = "/var/backup/postgresql";
    compression = "zstd";
    startAt = [];
  };

  services.restic.backups.nextcloud = {
    repository = "/var/backup/restic/nextcloud";
    passwordFile = config.sops.secrets.restic-password.path;
    initialize = true;

    paths = [
      "/var/backup/postgresql"
      "${nc.datadir}/config"
    ];
    # Group folders only exist once the first one is created in the admin UI.
    dynamicFilesFrom = ''
      [ -d ${nc.datadir}/data/__groupfolders ] && echo ${nc.datadir}/data/__groupfolders
      true
    '';

    timerConfig = {
      OnCalendar = "03:00";
      Persistent = true;
    };
    pruneOpts = [
      "--keep-daily 7"
      "--keep-weekly 4"
      "--keep-monthly 12"
    ];
    runCheck = true;
    checkOpts = ["--read-data-subset=10%"];
  };

  systemd.services.restic-backups-nextcloud = {
    wants = ["postgresqlBackup-${nc.config.dbname}.service"];
    after = ["postgresqlBackup-${nc.config.dbname}.service"];
  };

  sops.secrets.restic-password = {};
}
