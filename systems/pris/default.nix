{ config, pkgs, lib, ... }:

{
  networking.hostName = "pris";

  networking.networkmanager = {
    enable = true;
    plugins = with pkgs; [
      networkmanager-openvpn
    ];
  };

  environment.variables = {
    XDG_DATA_DIRS = [
      "/usr/share"
      "/var/lib/flatpak/exports/share"
      "$HOME/.local/share/flatpak/exports/share"
    ];
  };
  
  # Set your time zone.
  time.timeZone = "Europe/Rome";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_IE.UTF-8";
    LC_IDENTIFICATION = "en_IE.UTF-8";
    LC_MEASUREMENT = "en_IE.UTF-8";
    LC_MONETARY = "en_IE.UTF-8";
    LC_NAME = "en_IE.UTF-8";
    LC_NUMERIC = "en_IE.UTF-8";
    LC_PAPER = "en_IE.UTF-8";
    LC_TELEPHONE = "en_IE.UTF-8";
    LC_TIME = "en_IE.UTF-8";
  };

  programs.xfconf.enable = true;
  programs.dconf.enable = true;

  # OpenArena
  networking.firewall.allowedUDPPorts = [ 27960 27961 27962 27963 ];

  # Tailscale
  services.tailscale.enable = true;
  
  # XDG Portals
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk pkgs.xdg-desktop-portal-wlr];
    config.common.default = "*";
  };
  
  # Install all nerd fonts
  fonts.packages = builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);
  
  # Enable Flakes
  nix.settings.experimental-features = ["nix-command" "flakes"];

  # Enable Bluetooth
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  services.blueman.enable = true;

  # Enable Printing
  services.printing.enable = true;

  # Sound with Pipewire
  services.pulseaudio.enable = false;

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Security / Polkit
  security.polkit.enable = true;
  
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;

  programs.zsh.enable = true;
  users.users.lopl = {
    isNormalUser = true;
    description = "lopl";
    extraGroups = ["networkmanager" "wheel" "video" "audio" "input" "greeter" "libvirtd" "podman" "docker" "nm-openvpn"];
    shell = pkgs.zsh;
  };

  # Docker
  virtualisation.docker = {
    enable = true;
  };
 
  # libvirtd
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = true;
      swtpm.enable = true;
      vhostUserPackages = [pkgs.virtiofsd];
    };
  };

  # Virt-manager
  networking.firewall.trustedInterfaces = ["virbr0"];
  systemd.services.libvirt-default-network = {
    description = "Start libvirt default network";
    after = ["libvirtd.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.libvirt}/bin/virsh net-start default";
      ExecStop = "${pkgs.libvirt}/bin/virsh net-destroy default";
      User = "root";
    };
  };
  virtualisation.spiceUSBRedirection.enable = true;

  `
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  services.desktopManager.plasma6.enable = true;

  # Graphics
  hardware.graphics = {
    enable = true;
    enable32Bit = true; 
  };

  # GameMode  
  programs.gamemode.enable = true; 

  # Install steam with firewall configs--
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; 
    dedicatedServer.openFirewall = true; 
    gamescopeSession.enable = true;
  };

  
  services.xserver.videoDrivers =;

  services.flatpak.enable=true;
  
  hardware.nvidia = {
    modesetting.enable = true;

    powerManagement.enable = true;
    powerManagement.finegrained = false;

    open = false;

    nvidiaSettings = true;

    # package = config.boot.kernelPackages.nvidiaPackages.beta;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  environment.systemPackages = with pkgs; [
    distrobox
    vim
    wget
    git
    curl
    openssl
    pciutils
    usbutils
    wl-clipboard
    wayland-utils
    pamixer
    busybox
    playerctl
    pulseaudio
    libsecret
  ];

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };
  services.dbus.enable = true;

  system.stateVersion = "25.11";
}
