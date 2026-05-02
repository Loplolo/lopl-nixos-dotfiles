{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./base.nix
    ./sops.nix
    ./tailscale.nix
    ./cloudflare.nix
    ./services/caddy.nix
    ./services/glance.nix
    ./services/dnsmasq.nix
    ./services/nextcloud.nix
    ./services/adguard.nix
    ./services/stirling-pdf.nix
    ./services/immich.nix
    ./services/navidrome.nix
    ./services/searxng.nix
    ./services/syncthing.nix
    ./services/homeassistant.nix
    ./services/microbin.nix
    ./services/forgejo.nix
    ./services/matrix.nix
    ./services/minecraft-server.nix
    ./services/tg-captcha-bot.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "leon";
  networking.networkmanager.enable = true;

  users.users.lopl = {
    isNormalUser = true;
    description = "lopl";
    extraGroups = ["networkmanager" "wheel"];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF3JAfNH9UYSk1Vmf/TcZ8cpQiCpb8qjy9Qx2n21A16R lopl@pris"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAAuB78grSfPRlpVI4f4wzOjCidHECOeJm3sc5R978I3 lopl@rachael"
    ];
  };

  system.stateVersion = "25.11";
}
