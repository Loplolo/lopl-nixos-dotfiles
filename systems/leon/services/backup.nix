{
  config,
  lib,
  pkgs,
  ...
}: let
  # Only company data is backed up; personal users/repos are left out on purpose.
  company = {
    nextcloudUsers = []; # e.g. ["mario.rossi"]
    nextcloudGroupFolders = []; # ids from `nextcloud-occ groupfolders:list`
    forgejoOrgs = []; # e.g. ["camocondo"]
  };

  dumpDir = "/var/backup";
  ncDir = config.services.nextcloud.datadir;
  fj = config.services.forgejo;

  paths =
    [
      "${dumpDir}/postgresql"
      "${dumpDir}/forgejo"
      "${ncDir}/config"
      "${fj.stateDir}/custom"
    ]
    ++ map (u: "${ncDir}/data/${u}/files") company.nextcloudUsers
    ++ map (id: "${ncDir}/data/__groupfolders/${toString id}") company.nextcloudGroupFolders
    ++ map (o: "${fj.repositoryRoot}/${lib.toLower o}") company.forgejoOrgs;
in {
  # Nextcloud DB dump (also holds personal rows; can't be split per user).
  # No timer of its own: the restic job pulls it in so dump and backup stay in sync.
  services.postgresqlBackup = {
    enable = true;
    databases = [config.services.nextcloud.config.dbname];
    location = "${dumpDir}/postgresql";
    compression = "zstd";
    startAt = [];
  };

  services.restic.backups.company = {
    repository = "${dumpDir}/restic/company";
    passwordFile = config.sops.secrets.restic-company-password.path;
    initialize = true;

    # Skip paths that don't exist yet (e.g. a company user not created so far).
    dynamicFilesFrom = ''
      for p in ${lib.escapeShellArgs paths}; do
        [ -e "$p" ] && echo "$p"
      done
    '';

    backupPrepareCommand = ''
      install -d -m 700 ${dumpDir}/forgejo
      ${pkgs.sqlite}/bin/sqlite3 ${fj.database.path} ".backup '${dumpDir}/forgejo/forgejo.db'"
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

  systemd.services.restic-backups-company = {
    wants = ["postgresqlBackup-${config.services.nextcloud.config.dbname}.service"];
    after = ["postgresqlBackup-${config.services.nextcloud.config.dbname}.service"];
  };

  sops.secrets.restic-company-password = {};
}
