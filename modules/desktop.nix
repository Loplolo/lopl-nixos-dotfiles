{
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [./common.nix];

  nixpkgs.overlays = [
    inputs.vintagestory-nix.overlays.default
    (import ../pkgs)
  ];
  nixpkgs.config.allowBroken = true;

  users.users.lopl.extraGroups = ["video" "audio" "input" "nm-openvpn"];

  # X server
  services.xserver = {
    enable = true;

    xkb = {
      layout = "us";
      variant = "altgr-intl";
      options = "ctrl:nocaps";
    };

    displayManager.lightdm.enable = true;

    desktopManager.session = [
      {
        name = "home-manager";
        start = ''
          ${pkgs.runtimeShell} $HOME/.hm-xsession &
          waitPID=$!
        '';
      }
    ];
  };

  services.displayManager.defaultSession = "home-manager";
  services.autorandr.enable = true;
  services.libinput.enable = true;

  # Networking
  networking.networkmanager = {
    enable = true;
    plugins = [pkgs.networkmanager-openvpn];
  };

  networking.firewall = {
    allowedTCPPorts = [22000];
    allowedUDPPorts = [22000 21027];
  };

  # Audio
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  services.blueman.enable = true;

  # Printing
  services.printing.enable = true;

  # Fonts
  fonts.packages = builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

  # Portals
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = ["gtk"];
  };

  # File manager services
  programs.xfconf.enable = true;
  programs.dconf.enable = true;
  services.gvfs.enable = true;
  services.tumbler.enable = true;

  zramSwap.enable = true;

  hardware.graphics.enable = true;
  security.polkit.enable = true;
  services.dbus.enable = true;
  services.flatpak.enable = true;

  environment.systemPackages = with pkgs; [
    vim
    wget
    git
    curl
    openssl
    pciutils
    usbutils
    busybox
    xclip
    xrandr
    brightnessctl
    pamixer
    playerctl
    pulseaudio
    libsecret
    networkmanagerapplet
  ];
}
