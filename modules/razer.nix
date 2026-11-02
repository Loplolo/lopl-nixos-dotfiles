{pkgs, ...}: {
  hardware.openrazer = {
    enable = true;
    users = ["lopl"];
  };

  environment.systemPackages = [pkgs.polychromatic];
}
