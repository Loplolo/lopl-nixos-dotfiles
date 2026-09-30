{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./common.nix
    ./sops.nix
  ];

  options.lopl.proxies = lib.mkOption {
    type = lib.types.attrsOf lib.types.port;
    default = {};
  };

  config = {
    # SSH
    services.openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "no";
      };
    };

    virtualisation.podman = {
      enable = true;
      dockerCompat = true;
    };

    networking.nftables.enable = true;
    systemd.network.wait-online.enable = false;
    boot.initrd.systemd.network.wait-online.enable = false;

    services.btrfs.autoScrub = {
      enable = true;
      interval = "weekly";
      fileSystems = ["/"];
    };

    environment.systemPackages = with pkgs; [
      vim
      git
      wget
      curl
      htop
      btop
      tmux
      ncdu
      sops
      age
      ssh-to-age
      cloudflared
      fastfetch
      podman
      dig
      nmap
      tcpdump
      forgejo
      nextcloud-client
      lsof
      iotop
      smartmontools
      btrfs-progs
      lnav
      caddy
      mcrcon
    ];
  };
}
