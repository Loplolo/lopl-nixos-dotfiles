{config, ...}: let
  colors = config.lib.stylix.colors.withHashtag;
  transparent = "#00000000";
in {
  programs.i3status-rust = {
    enable = true;
    bars.default = {
      icons = "awesome6";
      theme = "plain";
      settings.theme.overrides = {
        idle_bg = transparent;
        idle_fg = colors.base05;
        info_bg = transparent;
        info_fg = colors.base0D;
        good_bg = transparent;
        good_fg = colors.base0B;
        warning_bg = transparent;
        warning_fg = colors.base0A;
        critical_bg = transparent;
        critical_fg = colors.base08;
        separator = "";
        separator_bg = transparent;
        separator_fg = colors.base05;
      };
      blocks = [
        {
          block = "cpu";
          format = " $icon $utilization.eng(w:1) ";
        }
        {
          block = "memory";
          format = " $icon $mem_used.eng(w:1,prefix:Gi)/$mem_total.eng(w:1,prefix:Gi) ($mem_used_percents) ";
        }
        {
          block = "temperature";
          format = " $icon $max ";
        }
        {
          block = "net";
          format = " $icon {$ssid ($signal_strength)|$device} ";
        }
        {
          block = "sound";
          format = " $icon {$volume.eng(w:1)|} ";
          click = [
            {
              button = "left";
              cmd = "pavucontrol";
            }
          ];
        }
        {
          block = "battery";
          missing_format = "";
        }
        {
          block = "time";
          interval = 60;
          format = " <b>$timestamp.datetime(f:'%a %d-%m-%Y %H:%M')</b> ";
        }
      ];
    };
  };
}
