{
  pkgs,
  inputs,
  ...
}: {
  stylix = {
    enable = true;
    image = ./quake.png;
    polarity = "dark";

    base16Scheme = {
      scheme = "Quake";
      base00 = "100d0a";
      base01 = "251f1a";
      base02 = "382e26";
      base03 = "504035";
      base04 = "8a7f76";
      base05 = "b0a498";
      base06 = "cbbdb0";
      base07 = "e0d5c8";
      base08 = "a53535";
      base09 = "c75a22";
      base0A = "c49548";
      base0B = "839c51";
      base0C = "668c8c";
      base0D = "4d6575";
      base0E = "854f68";
      base0F = "6b4e3a";
    };

    opacity = {
      desktop = 0.5;
      terminal = 0.9;
      popups = 1.0;
      applications = 1.0;
    };

    cursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Classic";
      size = 24;
    };

    fonts = {
      serif = {
        package = pkgs.noto-fonts;
        name = "Noto Serif";
      };
      sansSerif = {
        package = pkgs.open-sans;
        name = "Open Sans";
      };
      monospace = {
        package = pkgs.nerd-fonts.fira-code;
        name = "Fira Code";
      };

      sizes = {
        popups = 13;
        applications = 13;
        terminal = 15;
      };
    };
  };
  gtk = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
  };
  xfconf.settings = {
    thunar = {
      "last-view" = "ThunarIconView";
      "last-icon-view-zoom-level" = "THUNAR_ZOOM_LEVEL_100_PERCENT";
      "last-show-hidden" = true;
      "last-separator-position" = 200;
      "misc-single-click" = false;
    };
  };
}
