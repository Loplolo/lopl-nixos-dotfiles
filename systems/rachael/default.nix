{
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

  networking.hostName = "rachael";

  # Force font size and light mode on laptop only
  home-manager.users.lopl.stylix = {
    fonts.sizes.terminal = lib.mkForce 12;
    polarity = "light";
  };

  services.guix.enable = true;

  # TrackPoint
  services.xserver.inputClassSections = [
    ''
      Identifier "TrackPoint"
      MatchProduct "TPPS/2 Elan TrackPoint"
      Driver "libinput"
      Option "AccelSpeed" "0.75"
      Option "AccelProfile" "flat"
    ''
  ];

  services.xserver.deviceSection = ''Option "TearFree" "true"'';

  # Bootloader
  boot.initrd = {
    availableKernelModules = [
      "ahci"
      "xhci_pci"
      "sd_mod"
      "sr_mod"
    ];
    kernelModules = ["pinctrl_alderlake"];
  };

  # OpenArena
  networking.firewall.allowedUDPPorts = [27960 27961 27962 27963];

  # Fingerprint reader support
  services.fprintd = {
    enable = true;
    tod.driver = pkgs.libfprint-2-tod1-goodix;
  };
  security.pam.services = {
    polkit-1.fprintAuth = true;
    sudo.fprintAuth = true;
  };
  systemd.services.fprintd = {
    wantedBy = ["multi-user.target"];
    serviceConfig.Type = "simple";
  };

  # Enable Graphics and ipu6 webcam
  # intel integrated graphics
  hardware = {
    graphics = {
      enable32Bit = true;
      extraPackages = with pkgs; [
        vpl-gpu-rt
        intel-vaapi-driver
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

  # GameMode
  programs.gamemode.enable = true;

  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = "iHD"; # Prefer the modern iHD backend
  };

  hardware.enableRedistributableFirmware = true;
  boot.kernelParams = ["i915.enable_guc=3"];

  users.users.lopl.extraGroups = ["podman"];

  # Prefer IPv4
  networking.getaddrinfo = {
    enable = true;
    precedence = {
      "::ffff:0:0/96" = 100;
    };
  };

  environment.systemPackages = [pkgs.vial];
  # Vial udev rules
  services.udev = {
    packages = [pkgs.via];
    extraRules = ''
      KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{serial}=="*vial:f64c2b3c*", MODE="0660", GROUP="users", TAG+="uaccess", TAG+="udev-acl"
    '';
  };
  hardware.keyboard.qmk.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data were taken.
  system.stateVersion = "25.11";
}
