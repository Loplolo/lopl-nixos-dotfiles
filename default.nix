{
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "leon";
  networking.networkmanager.enable = true;

  sops = {
    defaultSopsFile = ../../secrets/secrets.yaml;
    validateSopsFiles = false;
    age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];

    secrets = {
      "tailscale-key" = {
        sopsFile = ../../secrets/tailscale.yaml;
      };
      "cloudflare-token" = {
        sopsFile = ../../secrets/cloudflare.yaml;
      };
    };
  };

  services.tailscale = {
    enable = true;
    authKeyFile = config.sops.secrets."tailscale-key".path;
  };

  sops.secrets.cloudflared-creds = {
    owner = "cloudflared";
  };

  services.cloudflared = {
    enable = true;
    tunnels = {
      "leon-tunnel" = {
        credentialsFile = config.sops.secrets.cloudflared-creds.path;
        default = "http_status:404";
      };
    };
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [22];
    trustedInterfaces = ["tailscale0"];
  };

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  services.btrfs.autoScrub = {
    enable = true;
    interval = "weekly";
    fileSystems = ["/"];
  };

  users.users.lopl = {
    isNormalUser = true;
    description = "lopl";
    extraGroups = ["networkmanager" "wheel" "docker"];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF3JAfNH9UYSk1Vmf/TcZ8cpQiCpb8qjy9Qx2n21A16R lopl@pris"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAAuB78grSfPRlpVI4f4wzOjCidHECOeJm3sc5R978I3 lopl@rachael"
    ];
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
    ssh-to-age
    age
  ];

  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 14d";
  };

  nix.settings.auto-optimise-store = true;

  system.stateVersion = "25.11";
}
