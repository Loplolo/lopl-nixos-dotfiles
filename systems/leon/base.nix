{
  config,
  pkgs,
  ...
}: {
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

  systemd.services.tailscaled.serviceConfig.Environment = [
    "TS_DEBUG_FIREWALL_MODE=nftables"
  ];
  systemd.network.wait-online.enable = false;
  boot.initrd.systemd.network.wait-online.enable = false;

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [22 80 443 53 3001 3000 25565 8123];
    allowedUDPPorts = [53 25565 5353];
    trustedInterfaces = ["tailscale0"];
    checkReversePath = "loose";
  };

  services.btrfs.autoScrub = {
    enable = true;
    interval = "weekly";
    fileSystems = ["/"];
  };

  nixpkgs.config.allowUnfree = true;
  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.settings.auto-optimise-store = true;
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
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
    matrix-conduit
    mcrcon
  ];
}
