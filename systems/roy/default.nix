{ config, pkgs, lib, ... }:

{
  imports =
    [ 
      ./tuigreet.nix
    ];
  
  
  
  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.availableKernelModules = [ 
    "virtio_blk"   
    "virtio_pci" 
    "ahci" 
    "xhci_pci" 
    "sd_mod" 
    "sr_mod" 
  ];
  #boot.loader.grub.enable = true;
  #boot.loader.grub.device = "/dev/vda";

  networking.hostName = "roy";
  networking.networkmanager.enable = true;

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
  
  # Fingerprint reader support
  #services.fprintd.enable = true;

  # XDG Portals 
  xdg.portal = {
    enable = true;
    wlr.enable = true; 
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
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
 
  # Enable Graphics 
  hardware.graphics.enable = true;

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
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Allow unfree packages 
  nixpkgs.config.allowUnfree = true;

  programs.zsh.enable = true;
  users.users.lopl = {
    isNormalUser = true;
    description = "lopl";
    extraGroups = [ "networkmanager" "wheel" "video" "audio" "input" "greeter" ];
    shell = pkgs.zsh; 
  };
  
  services.flatpak.enable = true;

  # VM specific guest additions  
  services.qemuGuest.enable = true;
  services.spice-vdagentd.enable = true;

  
  environment.systemPackages = with pkgs; [
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
  ];

  # Fonts required 
  fonts.packages = with pkgs; [
    fira-code
    font-awesome
    noto-fonts
    noto-fonts-color-emoji
  ];

  # Garbage collection
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data were taken.
  system.stateVersion = "25.05"; 
}
