{
  pkgs,
  config,
  lib,
  ...
}: let
  darkTheme = {
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

  lightTheme = {
    scheme = "Quake Light";
    base00 = "e0d5c8";
    base01 = "cbbdb0";
    base02 = "b0a498";
    base03 = "8a7f76";
    base04 = "504035";
    base05 = "382e26";
    base06 = "251f1a";
    base07 = "100d0a";
    base08 = "9e2a2a";
    base09 = "a84715";
    base0A = "9c6d29";
    base0B = "5e782e";
    base0C = "3f6e6e";
    base0D = "364c5c";
    base0E = "6b3851";
    base0F = "543a29";
  };

  wallpaper = ./wallpaper.png;
in {
  stylix = {
    enable = true;
    overlays.enable = false;
    image = wallpaper;

    base16Scheme =
      if config.stylix.polarity == "light"
      then lightTheme
      else darkTheme;

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
}
