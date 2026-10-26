{pkgs, ...}: {
  hardware.openrazer = {
    enable = false;
    users = ["lopl"];
  };

  environment.systemPackages = [pkgs.polychromatic];
}
