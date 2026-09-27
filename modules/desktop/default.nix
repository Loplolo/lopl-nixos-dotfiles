{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ../common
    ./audio.nix
    ./bluetooth.nix
    ./fonts.nix
    ./greetd.nix
    ./portals.nix
    ./printing.nix
    ./thunar.nix
  ];

  nixpkgs.overlays = [
    inputs.vintagestory-nix.overlays.default
    (import ../../pkgs)
  ];
  nixpkgs.config.allowBroken = true;

  users.users.lopl.extraGroups = ["video" "audio" "input" "greeter" "nm-openvpn"];

  networking.networkmanager = {
    enable = true;
    plugins = [pkgs.networkmanager-openvpn];
  };

  networking.firewall = {
    allowedTCPPorts = [22000];
    allowedUDPPorts = [22000 21027];
  };

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
    wl-clipboard
    wayland-utils
    brightnessctl
    pamixer
    playerctl
    pulseaudio
    libsecret
    networkmanagerapplet
  ];
}
