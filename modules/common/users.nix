{pkgs, ...}: {
  programs.zsh.enable = true;

  users.users.lopl = {
    isNormalUser = true;
    description = "lopl";
    extraGroups = ["networkmanager" "wheel"];
    shell = pkgs.zsh;
  };
}
