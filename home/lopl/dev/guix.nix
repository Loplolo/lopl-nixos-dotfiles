{config, ...}: {
  home.sessionPath = [
    "$HOME/.config/guix/current/bin"
    "$HOME/.guix-profile/bin"
  ];

  home.sessionVariables = {
    GUIX_PROFILE = "$HOME/.guix-profile";
    GUIX_LOCPATH = "$HOME/.guix-profile/lib/locale";
  };

  xdg.systemDirs.data = [
    "${config.home.homeDirectory}/.guix-profile/share"
  ];
}
