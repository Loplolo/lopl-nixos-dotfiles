{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../../modules/tailscale.nix
    ../../modules/nix-ld.nix
    ../../modules/razer.nix
    ../../modules/virtualisation.nix
  ];

  networking.hostName = "pris";

  home-manager.users.lopl.xsession.initExtra = lib.mkBefore ''
    xrandr --output DP-0 --primary --pos 0x0 --output HDMI-0 --pos 1920x0 --rotate left
  '';

  # VR
  services.wivrn = {
    enable = true;
    openFirewall = true;
    autoStart = true;
    package = pkgs.wivrn.override {cudaSupport = true;};
  };

  # for kinect
  services.udev = {
    extraRules = ''
      SUBSYSTEM=="usb", ATTR{idVendor}=="045e", ATTR{idProduct}=="02b0", MODE="0666", TAG+="uaccess"
      SUBSYSTEM=="usb", ATTR{idVendor}=="045e", ATTR{idProduct}=="02ad", MODE="0666", TAG+="uaccess"
      SUBSYSTEM=="usb", ATTR{idVendor}=="045e", ATTR{idProduct}=="02ae", MODE="0666", TAG+="uaccess"
    '';
    packages = [pkgs.game-devices-udev-rules pkgs.xwiimote];
  };

  services.guix.enable = true;

  # Sound with Pipewire
  services.pipewire = {
    wireplumber.enable = true;
    jack.enable = true;
  };

  networking.firewall = {
    allowedUDPPorts = [27960 27961 27962 27963 9757];
    allowedTCPPorts = [9757];
  };

  networking.nftables.enable = true;
  services.resolved.enable = true;

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

  # Enable Bluetooth
  hardware.bluetooth.settings.General = {
    Experimental = true;
    KernelExperimental = true;
    FastConnectable = true;
    Enable = "Source,Sink,Media,Socket";
  };

  # Enable Printing
  services.printing.drivers = with pkgs; [
    cups-filters
    cups-browsed
    epson-escpr
  ];

  services.ipp-usb.enable = true;

  users.users.lopl.extraGroups = ["podman" "adbusers" "render" "lp" "uinput"];

  # WayDroid
  virtualisation.waydroid.enable = true;

  # Graphics
  hardware.graphics = {
    enable32Bit = true;
    extraPackages = with pkgs; [
      nvidia-vaapi-driver
    ];
  };
  boot.kernelParams = [
    "nvidia-drm.modeset=1"
    "nvidia-drm.fbdev=1"
    "btusb.enable_autosuspend=0"
  ];
  hardware.nvidia-container-toolkit.enable = true;

  # GameMode
  programs.gamemode.enable = true;
  programs.gamescope.enable = true;

  # Steam with firewall configs
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    gamescopeSession.enable = true;
  };

  hardware.cpu.amd.updateMicrocode = true;

  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    open = true;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  environment.variables = {
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    __GL_GSYNC_ALLOWED = "1";
    __GL_VRR_ALLOWED = "1";
    LIBVA_DRIVER_NAME = "nvidia";
  };

  # xserver
  services.xserver.videoDrivers = ["nvidia"];

  environment.systemPackages = with pkgs; [
    wayvr

    libX11

    libGL
    libGLU
    vulkan-loader
    vulkan-tools

    mangohud

    xdg-utils
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";

  boot.initrd = {
    availableKernelModules = ["xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" "sr_mod"];
    kernelModules = ["nvidia" "nvidia_modeset" "nvidia_uvm" "nvidia_drm"];
    supportedFilesystems = ["btrfs"];
  };

  boot.kernelModules = ["kvm-amd" "binder_linux" "hid-wiimote" "uinput"];
  boot.extraModulePackages = with config.boot.kernelPackages; [
    v4l2loopback
  ];
  boot.extraModprobeConfig = ''
    options v4l2loopback devices=1 video_nr=9 card_label="OBS Virtual Camera" exclusive_caps=1 max_buffers=8
  '';

  system.stateVersion = "25.11";
}
