{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./tuigreet.nix
  ];
  environment.variables = {
    XDG_DATA_DIRS = [
      "/usr/share"
      "/var/lib/flatpak/exports/share"
      "$HOME/.local/share/flatpak/exports/share"
    ];
  };

  services.xserver.enable = true;

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.availableKernelModules = [
    "ahci"
    "xhci_pci"
    "sd_mod"
    "sr_mod"
  ];

  boot.initrd.kernelModules = ["pinctrl_alderlake"];

  # OpenArena
  networking.firewall.allowedUDPPorts = [ 27960 27961 27962 27963 ];
  
  # Tailscale
  services.tailscale.enable = true;

  # Fingerprint reader support
  services.fprintd.enable = true;
  security.pam.services.polkit-1.fprintAuth = true;
  security.pam.services.sudo.fprintAuth = true;
  services.fprintd.tod.driver = pkgs.libfprint-2-tod1-goodix;
  systemd.services.fprintd = {
    wantedBy = ["multi-user.target"];
    serviceConfig.Type = "simple";
  };

  networking.hostName = "rachael";

  networking.networkmanager = {
    enable = true;
    plugins = with pkgs; [
      networkmanager-openvpn
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

  # XDG Portals
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk pkgs.xdg-desktop-portal-wlr];
    config.common.default = "*";
  };
  # Extra thunar stuff
  programs.thunar.plugins = with pkgs.xfce; [
    thunar-archive-plugin
    thunar-volman
  ];

  services.gvfs.enable = true;
  services.tumbler.enable = true;

  programs.thunar.enable = true;
  programs.xfconf.enable = true;
  programs.dconf.enable = true;

  # Enable Graphics and ipu6 webcam
  # intel integrated graphics
  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs; [
        vpl-gpu-rt
        intel-media-driver
      ];
    };
    firmware = [
      pkgs.ipu6-camera-bins
      pkgs.ivsc-firmware
    ];
    ipu6 = {
      enable = true;
      platform = "ipu6ep";
    };
  };

  # Install all nerd fonts
  fonts.packages = builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = "iHD"; # Prefer the modern iHD backend
    # VDPAU_DRIVER = "va_gl";      # Only if using libvdpau-va-gl
  };

  hardware.enableRedistributableFirmware = true;
  boot.kernelParams = ["i915.enable_guc=3"];

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

  # Enable Flakes
  nix.settings.experimental-features = ["nix-command" "flakes"];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;

  programs.zsh.enable = true;
  users.users.lopl = {
    isNormalUser = true;
    description = "lopl";
    extraGroups = ["networkmanager" "wheel" "video" "audio" "input" "greeter" "libvirtd" "podman" "docker" "nm-openvpn"];
    shell = pkgs.zsh;
  };


  # Flatpak
  services.flatpak.enable = true;

  # Docker
  virtualisation.docker = {
    enable = true;
  };

  # Steam
  programs.steam = {
 	  enable = true;

	  remotePlay.openFirewall = true; 
	  dedicatedServer.openFirewall = true;
	  localNetworkGameTransfers.openFirewall = true;
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

  environment.systemPackages = with pkgs; [
    distrobox
    vim
    wget
    git
    curl
    pciutils
    usbutils
    wl-clipboard
    wayland-utils
    brightnessctl
    pamixer
    networkmanagerapplet
    busybox
    xfce.thunar
    playerctl
    pulseaudio
  ];

  # Garbage collection
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data were taken.
  system.stateVersion = "25.11";
}
