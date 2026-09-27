{pkgs, ...}: {
  services.xserver = {
    enable = true;

    xkb = {
      layout = "us";
      variant = "altgr-intl";
      options = "ctrl:nocaps";
    };

    displayManager.lightdm.enable = true;

    desktopManager.session = [
      {
        name = "home-manager";
        start = ''
          ${pkgs.runtimeShell} $HOME/.hm-xsession &
          waitPID=$!
        '';
      }
    ];
  };

  services.displayManager.defaultSession = "home-manager";
  services.libinput.enable = true;
}
