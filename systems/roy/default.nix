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
  #boot.loader.grub.enable = true;
  #boot.loader.grub.device = "/dev/vda";

  networking.hostName = "roy";

  # Fingerprint reader support
  #services.fprintd.enable = true;

  # VM specific guest additions
  services.qemuGuest.enable = true;
  services.spice-vdagentd.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data were taken.
  system.stateVersion = "25.05";
}
