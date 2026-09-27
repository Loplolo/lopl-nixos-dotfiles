{
  # Bootloader
  boot.initrd.availableKernelModules = [
    "virtio_blk"
    "virtio_pci"
    "ahci"
    "xhci_pci"
    "sd_mod"
    "sr_mod"
  ];

  networking.hostName = "roy";

  home-manager.users.lopl.wayland.windowManager.sway.config.output."Virtual-1".mode = "1920x1080@60Hz";

  # VM specific guest additions
  services.qemuGuest.enable = true;
  services.spice-vdagentd.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data were taken.
  system.stateVersion = "25.05";
}
