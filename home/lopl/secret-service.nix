{ pkgs, ... }:
{
  programs.keepassxc = {
    autostart = true;
    enable = true;
    settings = {
      FdoSecrets.Enabled = true;
    };
  };

  xdg.autostart.enable = true; 

}
