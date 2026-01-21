{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    inputs.nix-flatpak.homeManagerModules.nix-flatpak
    inputs.stylix.homeModules.stylix

    ./packages.nix
    ./theme.nix
    ./flatpak.nix
    ./firefox.nix
    ./nyxt.nix
    ./git.nix
    ./shell.nix
    ./sway.nix
    ./tofi.nix
    ./waybar.nix
    ./emacs
    ./xdg_mime.nix
  ];

  

  home.username = "lopl";
  home.homeDirectory = "/home/lopl";
  home.stateVersion = "25.11";

  programs.home-manager.enable = true;
}
