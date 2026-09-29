{inputs, ...}: {
  imports = [
    inputs.nix-flatpak.homeManagerModules.nix-flatpak
    inputs.stylix.homeModules.stylix
    ./emacs
    ./firefox.nix
    ./flatpak.nix
    ./git.nix
    ./gpg.nix
    ./guix.nix
    ./i3.nix
    ./i3status-rust.nix
    ./nyxt.nix
    ./packages.nix
    ./programming.nix
    ./secret-service.nix
    ./shell.nix
    ./syncthing.nix
    ./theme.nix
    ./thunar.nix
    ./xdg-mime.nix
  ];

  home.username = "lopl";
  home.homeDirectory = "/home/lopl";
  home.stateVersion = "25.11";
  programs.home-manager.enable = true;
}
