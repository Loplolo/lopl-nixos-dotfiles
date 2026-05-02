{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./tuigreet.nix
  ];

  networking.hostName = "pris";

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

  # VR
  services.wivrn = {
    enable = true;
    openFirewall = true;
    autoStart = true;
    package = pkgs.wivrn.override {cudaSupport = true;};
  };

  # nix-ld
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    libGL
    libGLU
    libX11
    libXcursor
    libXrandr
    libXi
    alsa-lib
    stdenv.cc.cc.lib
    zlib
    openssl
  ];

  # XDG Portals
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-wlr];
    config.common.default = "*";
  };
  # Extra thunar stuff
  programs.thunar.plugins = with pkgs; [
    thunar-archive-plugin
    thunar-volman
  ];

  services.guix.enable = true;

  services.gvfs.enable = true;
  services.tumbler.enable = true;

  programs.thunar.enable = true;
  programs.xfconf.enable = true;
  programs.dconf.enable = true;

  # Sound with Pipewire
  services.pulseaudio.enable = false;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # OpenArena
  networking.firewall.allowedUDPPorts = [27960 27961 27962 27963];
  # Syncthing
  services.syncthing.openDefaultPorts = true;

  # Sops
  sops = {
    defaultSopsFile = ../../secrets/secrets.yaml;
    validateSopsFiles = false;
    age.keyFile = "/var/lib/sops-nix/key.txt";
    secrets.tailscale-authkey = {};
  };

  # Tailscale
  services.tailscale = {
    enable = true;
    authKeyFile = config.sops.secrets.tailscale-authkey.path;
  };

  networking.firewall.trustedInterfaces = ["tailscale0" "virbr0"];

  # quad9 dns
  networking.nameservers = ["9.9.9.9" "149.112.112.112"];

  networking.nftables.enable = true;

  # Mount HDD
  systemd.tmpfiles.rules = [
    "d /mnt/hdd0 0777 lopl users -"
  ];

  fileSystems."/mnt/hdd0" = {
    device = "/dev/disk/by-uuid/d6669637-5628-4064-b39a-c2d19dd3e7eb";
    fsType = "btrfs";
    options = [
      "defaults"
      "nofail"
      "x-systemd.automount"
      "compress=zstd"
    ];
  };

  # Install all nerd fonts
  fonts.packages = builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

  # Enable Flakes
  nix.settings = {
    experimental-features = ["nix-command" "flakes"];
    auto-optimise-store = true;
  };

  # Enable Bluetooth
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  services.blueman.enable = true;

  # Enable Printing
  services.printing.enable = true;

  security.rtkit.enable = true;

  # Security / Polkit
  security.polkit.enable = true;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;

  programs.zsh.enable = true;
  users.users.lopl = {
    isNormalUser = true;
    description = "lopl";
    extraGroups = ["networkmanager" "wheel" "video" "audio" "input" "greeter" "libvirtd" "podman" "docker" "nm-openvpn" "adbusers" "render"];
    shell = pkgs.zsh;
  };

  # WayDroid
  virtualisation.waydroid.enable = true;

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

  # Graphics
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
  boot.kernelParams = [
    "nvidia-drm.modeset=1"
    "nvidia-drm.fbdev=1"
  ];
  hardware.nvidia.modesetting.enable = true;

  # GameMode
  programs.gamemode.enable = true;
  programs.gamescope.enable = true;

  # Install steam with firewall configs
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    gamescopeSession.enable = true;
  };

  services.ollama = {
    enable = true;
    loadModels = ["deepseek-r1:8b" "qwen3:8b"];
  };

  services.xserver.videoDrivers = ["nvidia"];

  services.flatpak.enable = true;
  hardware.cpu.amd.updateMicrocode = true;

  hardware.nvidia = {
    powerManagement.enable = true;
    powerManagement.finegrained = false;
    open = true;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  environment.variables = {
    GBM_BACKEND = "nvidia-drm";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    NIXOS_OZONE_WL = "1";
    __GL_GSYNC_ALLOWED = "1";
    __GL_VRR_ALLOWED = "1";
  };

  # xserver
  services.xserver.enable = true;

  environment.systemPackages = with pkgs; [
    vim
    wget
    git
    curl
    openssl
    pciutils
    usbutils
    wl-clipboard
    wayvr
    wayland-utils
    pamixer
    busybox
    playerctl
    pulseaudio
    libsecret
    usbutils
    pciutils

    libX11

    libGL
    libGLU
    vulkan-loader
    vulkan-tools

    mangohud

    mako
    networkmanagerapplet
    brightnessctl
    thunar
  ];

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };
  services.dbus.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_zen;
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";

  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" "sr_mod"];
  boot.initrd.kernelModules = ["nvidia" "nvidia_modeset" "nvidia_uvm" "nvidia_drm"];

  boot.kernelModules = ["kvm-amd" "binder_linux"];
  boot.extraModulePackages = with config.boot.kernelPackages; [
    v4l2loopback
  ];
  boot.extraModprobeConfig = ''
    options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
  '';

  boot.loader.systemd-boot.enable = true;
  boot.initrd.supportedFilesystems = ["btrfs"];
  system.stateVersion = "25.11";

  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 32768;
    }
  ];

  nixpkgs.config.permittedInsecurePackages = [
    "electron-38.8.4"
  ];

  nix.settings = {
    max-jobs = "auto";
    cores = 0;
    http-connections = 50;
  };
}
