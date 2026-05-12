{
  config,
  pkgs,
  ...
}: {
  users = {
    users = {
      jellyfin = {
        isSystemUser = true;
        createHome = true;
        group = "jellyfin";
        home = "/var/lib/jellyfin";
      };
    };
  };
  users.groups.jellyfin = {};

  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };
}
