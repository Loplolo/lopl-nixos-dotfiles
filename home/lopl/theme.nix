{
  pkgs,
  config,
  lib,
  inputs,
  ...
}: let
  polarity = config.stylix.polarity;
  scheme = variant: "${pkgs.base16-schemes}/share/themes/flexoki-${variant}.yaml";
  base16 = inputs.stylix.inputs.base16.lib {inherit pkgs lib;};

  wallpaper = ./wallpaper.png;
in {
  lib.lopl.barColors = (base16.mkSchemeAttrs (scheme "dark")).withHashtag;

  stylix = {
    enable = true;
    overlays.enable = false;
    image = wallpaper;

    polarity = lib.mkDefault "dark";
    base16Scheme = scheme polarity;
    override = lib.mkIf (polarity == "light") {
      base03 = "878580";
      base04 = "6F6E69";
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
      sansSerif = {
        package = pkgs.fira-sans;
        name = "Fira Sans";
      };
      serif = {
        package = pkgs.merriweather;
        name = "Merriweather";
      };
      monospace = {
        package = pkgs.nerd-fonts.fira-code;
        name = "FiraCode Nerd Font";
      };
      emoji = {
        package = pkgs.noto-fonts-color-emoji;
        name = "Noto Color Emoji";
      };
    };

    icons = {
      enable = true;
      package = pkgs.papirus-icon-theme;
      dark = "Papirus-Dark";
      light = "Papirus-Light";
    };
  };
}
