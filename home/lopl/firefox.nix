{pkgs, ...}: {
  programs.firefox = {
    enable = true;

    profiles.lopl = {
      isDefault = true;

      settings = {
        "gfx.webrender.all" = true;
        "media.ffmpeg.vaapi.enabled" = true;
        "widget.use-xdg-desktop-portal.file-picker" = 1;
        "extensions.pocket.enabled" = false;
        "dom.disable_beforeunload" = true;
        "privacy.trackingprotection.enabled" = true;
      };
    };
  };
}
