{config, ...}: {
  programs.librewolf = {
    enable = true;
    profiles.lopl = {
      isDefault = true;

      settings = {
        "gfx.webrender.all" = true;
        "browser.startup.homepage" = "https://home.lopl.dev/";
        "media.ffmpeg.vaapi.enabled" = true;
        "extensions.pocket.enabled" = false;
        "dom.disable_beforeunload" = true;
        "privacy.trackingprotection.enabled" = true;
        "widget.use-aspect-ratio" = false;
      };
    };
  };

  stylix.targets.firefox.profileNames = ["lopl"];
}
