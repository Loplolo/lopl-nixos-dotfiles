{
  imports = [
    ./tailscale.nix
    ./services/cloudflared.nix
    ./services/caddy.nix
    ./services/nextcloud.nix
    ./services/adguard.nix
    ./services/immich.nix
    ./services/navidrome.nix
    ./services/syncthing.nix
    ./services/homeassistant.nix
    ./services/microbin.nix
    ./services/forgejo.nix
    ./services/collabora.nix
    ./services/backup.nix
    #./services/minecraft-server.nix
    ./services/tg-captcha-bot.nix
    #./services/blog.nix
  ];

  networking.hostName = "leon";
  networking.networkmanager.enable = true;

  nix.settings.trusted-users = [
    "root"
    "@wheel"
  ];

  users.users.lopl = {
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF3JAfNH9UYSk1Vmf/TcZ8cpQiCpb8qjy9Qx2n21A16R lopl@pris"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAAuB78grSfPRlpVI4f4wzOjCidHECOeJm3sc5R978I3 lopl@rachael"
    ];
  };

  fileSystems."/mnt/storage" = {
    device = "/dev/disk/by-uuid/77DB-F28C";
    fsType = "exfat";
    options = [
      "defaults"
      "nofail"
      "uid=1000"
      "gid=100"
      "dmask=0002"
      "fmask=0002"
    ];
  };
  system.stateVersion = "25.11";
}
