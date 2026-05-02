{
  pkgs,
  config,
  ...
}: {
  programs.firefox = {
    enable = true;
    configPath = "${config.xdg.configHome}/mozilla/firefox";
    profiles.lopl = {
      isDefault = true;

      settings = {
        "gfx.webrender.all" = true;
        "browser.startup.homepage" = "https://home.lopl.dev/";
        "media.ffmpeg.vaapi.enabled" = true;
        "extensions.pocket.enabled" = false;
        "dom.disable_beforeunload" = true;
        "privacy.trackingprotection.enabled" = true;
      };
    };
  };

  stylix.targets.firefox.profileNames = ["lopl"];
}
