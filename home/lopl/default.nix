{inputs, ...}: {
  imports = [
    inputs.nix-flatpak.homeManagerModules.nix-flatpak
    inputs.stylix.homeModules.stylix
    ./desktop
    ./dev
    ./firefox.nix
    ./flatpak.nix
    ./git.nix
    ./gpg.nix
    ./nyxt.nix
    ./packages.nix
    ./secret-service.nix
    ./shell.nix
    ./syncthing.nix
  ];

  home.username = "lopl";
  home.homeDirectory = "/home/lopl";
  home.stateVersion = "25.11";
  programs.home-manager.enable = true;
}
