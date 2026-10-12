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

  services.autorandr = {
    enable = true;
    defaultTarget = "default";
    profiles = {
      default = {
        fingerprint = {
          DP-0 = "00ffffffffffff0006b38f2701010101101f0104b53c22783b9b95a5574ea3260c5054b7ef0081c081cf8100810fd1c0d1d9d1e8d1fc50f5807870381440303ca20056502100003a000000fd0030f0ffff42010a202020202020000000fc005647323739514d0a2020202020000000ff004d344c4d51533133303135310a01b7020328f14d90010203111213040e0f1d1e1f2309070783010000e305e001e6060701606000e2006afc7e8088703812401820350056502100001e23e88078703887402020980c56502100001a023a801871382d40582c450056502100001e0000000000000000000000000000000000000000000000000000000000000000002a";
          HDMI-0 = "00ffffffffffff004c2d1a0d564c5a5a161a010380301b782a5295a556549d250e5054bfef80714f81c0810081809500a9c0b3000101023a801871382d40582c4500dd0c1100001e000000fd00324b1e5111000a202020202020000000fc00533232463335300a2020202020000000ff0048345a483630323131390a202001fc020311b14690041f13120365030c001000011d00bc52d01e20b8285540dd0c1100001e8c0ad090204031200c405500dd0c110000188c0ad08a20e02d10103e9600dd0c11000018000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050";
        };
        config = {
          DP-0 = {
            enable = true;
            crtc = 0;
            mode = "1920x1080";
            primary = true;
            position = "0x0";
            rate = "279.86";
          };
          HDMI-0 = {
            crtc = 1;
            mode = "1920x1080";
            position = "1920x0";
            rate = "60.00";
            rotate = "left";
          };
        };
      };
    };
  };
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
    powerManagement.enable = true;

    open = true;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };
  powerManagement.resumeCommands = ''
    sleep 3
      ${pkgs.autorandr}/bin/autorandr --batch --change --force --default default
  '';

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
