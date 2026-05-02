{
  config,
  pkgs,
  lib,
  osConfig,
  inputs,
  ...
}: {
  imports = [
    inputs.nix-flatpak.homeManagerModules.nix-flatpak
    inputs.stylix.homeModules.stylix
    ./git.nix
    ./gpg.nix
    ./shell.nix
    ./packages.nix
    ./firefox.nix
    ./theme.nix
    ./xdg_mime.nix
    ./flatpak.nix
    ./nyxt.nix
    ./secret-service.nix
    ./syncthing.nix
    ./emacs
    ./sway.nix
    ./waybar.nix
    ./tofi.nix
  ];

  home.username = "lopl";
  home.homeDirectory = "/home/lopl";
  home.stateVersion = "25.11";
  programs.home-manager.enable = true;
}
