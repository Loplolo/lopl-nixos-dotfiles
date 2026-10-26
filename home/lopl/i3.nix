{
  config,
  lib,
  pkgs,
  ...
}: let
  mod = "Mod4";
  colors = config.lib.stylix.colors.withHashtag;
  fonts = config.stylix.fonts;
  transparent = "#00000000";
  launcher = pkgs.writeShellScript "rofi-top-bar" ''
    exec rofi -show "$1" -theme-str '
      window { location: north; anchor: north; width: calc(100% - 16px); y-offset: 8px; }
      mainbox { orientation: horizontal; children: [ inputbar, listview ]; }
      inputbar { width: 25em; expand: false; }
      listview { layout: horizontal; lines: 100; spacing: 15px; fixed-height: false; scrollbar: false; }
      element-icon { enabled: false; }
    '
  '';
  menu = "${launcher} run";
  drun = "${launcher} drun";
  app = "${mod}+Control";
  exitPrompt = pkgs.writeShellScript "i3-exit-prompt" ''
    if [ "$(printf 'No\nYes' | rofi -dmenu -i -p 'Exit i3?')" = "Yes" ]; then
      i3-msg exit
    fi
  '';
in {
  programs.alacritty = {
    enable = true;
    settings.window = {
      padding = {
        x = 12;
        y = 12;
      };
      dynamic_padding = true;
    };
  };

  stylix.targets.rofi.alternatePattern = false;

  programs.rofi = {
    enable = true;
    settings.terminal = "alacritty";
    theme = let
      inherit (config.lib.formats.rasi) mkLiteral;
    in {
      window = {
        border = 0;
        padding = mkLiteral "8px";
      };
      inputbar.padding = mkLiteral "6px 10px";
      element = {
        padding = mkLiteral "6px 10px";
        border-radius = mkLiteral "6px";
      };
    };
  };

  services.dunst = {
    enable = true;
    settings.global = {
      origin = "top-right";
      offset = "(8, 8)";
      width = 360;
      padding = 12;
      horizontal_padding = 14;
      gap_size = 8;
      frame_width = 2;
      corner_radius = 10;
      icon_corner_radius = 6;
      progress_bar_corner_radius = 4;
      separator_height = 0;
    };
  };
  services.picom = {
    enable = true;
    backend = "glx";
    vSync = true;
    fade = true;
    fadeDelta = 4;
    shadow = true;
    shadowOpacity = 0.3;
    shadowExclude = [
      "window_type = 'dock'"
      "window_type = 'desktop'"
      "class_g = 'flameshot'"
      "_GTK_FRAME_EXTENTS@"
    ];
    settings = {
      corner-radius = 10;
      shadow-radius = 16;
      rounded-corners-exclude = [
        "window_type = 'dock'"
        "window_type = 'desktop'"
        "fullscreen"
      ];
    };
  };

  xsession = {
    enable = true;
    scriptPath = ".hm-xsession";
    initExtra = "${pkgs.xsetroot}/bin/xsetroot -cursor_name left_ptr";

    windowManager.i3 = {
      enable = true;
      config = {
        modifier = mod;
        terminal = "alacritty";

        gaps = {
          inner = 8;
        };

        window = {
          border = 0;
          titlebar = false;
          commands = [
            # Flameshot fix for multiple screens
            {
              criteria = {
                class = "flameshot";
              };
              command = "border pixel 0, floating enable, fullscreen disable, move absolute position 0 0";
            }
          ];
        };

        floating = {
          border = 0;
          titlebar = false;
        };

        # Bar configuration
        bars = [
          {
            position = "bottom";
            command = "${config.xsession.windowManager.i3.package}/bin/i3bar --transparency";
            statusCommand = "i3status-rs ${config.xdg.configHome}/i3status-rust/config-default.toml";
            trayOutput = "primary";
            trayPadding = 5;
            fonts = {
              names = [fonts.monospace.name "Font Awesome 6 Free Solid" "Font Awesome 6 Brands"];
              style = "Medium";
              size = fonts.sizes.desktop * 1.25;
            };
            colors = {
              background = transparent;
              statusline = colors.base05;
              separator = transparent;
              focusedWorkspace = {
                border = colors.base0D;
                background = transparent;
                text = colors.base05;
              };
              activeWorkspace = {
                border = transparent;
                background = transparent;
                text = colors.base05;
              };
              inactiveWorkspace = {
                border = transparent;
                background = transparent;
                text = colors.base04;
              };
              urgentWorkspace = {
                border = colors.base08;
                background = colors.base08;
                text = colors.base00;
              };
              bindingMode = {
                border = colors.base0E;
                background = colors.base0E;
                text = colors.base00;
              };
            };
          }
        ];

        workspaceOutputAssign = [
          {
            workspace = "1";
            output = "primary";
          }
        ];

        # Startup programs
        startup = [
          {
            command = "blueman-applet";
            notification = false;
          }
          {
            command = "nm-applet";
            notification = false;
          }
          {
            command = "i3-msg workspace number 1";
            notification = false;
          }
        ];
        keybindings = lib.mkOptionDefault {
          "${mod}+w" = null;

          # Basics
          "${mod}+Return" = "exec alacritty";
          "${mod}+Shift+q" = "kill";
          "${mod}+d" = "exec ${drun}";
          "${mod}+Shift+d" = "exec ${menu}";

          # Reload/Exit
          "${mod}+Shift+r" = "reload";
          "${mod}+Shift+e" = "exec --no-startup-id ${exitPrompt}";

          # Run autorandr to fix screens
          "${mod}+Shift+m" = "exec --no-startup-id autorandr --change --force";

          # Custom Apps
          "${app}+g" = "exec nyxt";
          "${app}+f" = "exec librewolf";
          "${app}+t" = "exec Telegram";
          "${app}+r" = "exec thunderbird";
          "${app}+c" = "exec code";
          "${app}+m" = "exec alacritty -e cmus";
          "${app}+s" = "exec steam";
          "${app}+e" = "exec emacsclient -c";
          "${app}+q" = "exec qss-m -basedir /home/lopl/.q1/";
          "${app}+w" = "exec wike";
          "${app}+x" = "exec xournalpp";
          "${app}+o" = "exec obsidian";
          "${app}+p" = "exec super-productivity";

          # Screenshots
          "--release ${mod}+Shift+s" = "exec maim -s | xclip -selection clipboard -t image/png";

          # Audio
          "XF86AudioRaiseVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ +10%";
          "XF86AudioLowerVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ -10%";
          "XF86AudioMute" = "exec pactl set-sink-mute @DEFAULT_SINK@ toggle";
          "XF86AudioMicMute" = "exec pactl set-source-mute @DEFAULT_SOURCE@ toggle";

          # Brightness
          "XF86MonBrightnessUp" = "exec brightnessctl set 5%+";
          "XF86MonBrightnessDown" = "exec brightnessctl set 5%-";
        };
      };

      extraConfig = ''
        default_orientation horizontal

        # Passthrough mode
        mode "passthrough" {
            bindsym Pause mode default
            bindsym ${mod}+Escape mode "default"
        }
        bindsym ${mod}+Escape mode passthrough
      '';
    };
  };
}
