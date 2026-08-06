{
  pkgs,
  lib,
  ...
}: let
  mod = "Mod4";
  menu = "tofi-run | xargs swaymsg exec --";
  drun = "tofi-drun | xargs swaymsg exec --";
  app = "${mod}+Control";
in {
  wayland.windowManager.sway = {
    enable = true;
    systemd.enable = true;
    wrapperFeatures.gtk = true;
    config = {
      modifier = mod;
      terminal = "alacritty";

      output = lib.mkMerge [
        {
          "*" = {
            bg = "${./wallpaper.png} fill";
          };

          "Virtual-1" = {
            mode = "1920x1080@60Hz";
          };

          "DP-1" = {position = "0 0";};
          "HDMI-A-1" = {
            position = "1920 0";
            transform = "270";
            subpixel = "vrgb";
          };
        }
      ];

      input = {
        "type:keyboard" = {
          xkb_layout = "us";
          xkb_variant = "altgr-intl";
          xkb_options = "ctrl:nocaps";
        };

        "2:10:TPPS/2_Elan_TrackPoint" = {
          dwt = "disabled";
          tap = "enabled";
          pointer_accel = "0.75";
          accel_profile = "flat";
        };
      };

      gaps = {
        inner = 8;
      };

      window = {
        border = 0;
        titlebar = false;
        commands = [
          # Flameshot fix for multiple screens
          {
            criteria = {app_id = "flameshot";};
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
          command = "waybar";
          position = "bottom";
          #          colors = {
          #            background = "#000000";
          #            statusline = "#FFFFFF";
          #            separator  = "#666666";
          #            focusedWorkspace  = { border = "#83CAFA"; background = "#51A2DA"; text = "#FFFFFF"; };
          #            activeWorkspace   = { border = "#3C6EB4"; background = "#294172"; text = "#FFFFFF"; };
          #            inactiveWorkspace = { border = "#8C8C8C"; background = "#4C4C4C"; text = "#888888"; };
          #            urgentWorkspace   = { border = "#EC69A0"; background = "#DB3279"; text = "#FFFFFF"; };
          #            bindingMode       = { border = "#b691d3"; background = "#A07CBC"; text = "#FFFFFF"; };
          #         };
        }
      ];

      # Startup programs
      startup = [
        {
          command = "blueman-applet";
          always = true;
        }
        {
          command = "nm-applet --indicator";
          always = true;
        }
        {command = "swaymsg workspace 1";}
      ];
      keybindings = lib.mkOptionDefault {
        "${mod}+w" = "null";

        # Basics
        "${mod}+Return" = "exec alacritty";
        "${mod}+Shift+q" = "kill";
        "${mod}+d" = "exec ${drun}";
        "${mod}+Shift+d" = "exec ${menu}";

        # Reload/Exit
        "${mod}+Shift+r" = "reload";
        "${mod}+Shift+e" = "exec swaynag -t warning -m 'You pressed the exit shortcut.' -B 'Yes, exit sway' 'swaymsg exit'";

        # Custom Apps (Now using Super + Control)
        "${app}+g" = "exec nyxt";
        "${app}+f" = "exec firefox";
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
        "--release ${mod}+Shift+s" = "exec grim -g \"$(slurp)\" - | wl-copy";

        # Audio
        "XF86AudioRaiseVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ +10%";
        "XF86AudioLowerVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ -10%";
        "XF86AudioMute" = "exec pactl set-sink-mute @DEFAULT_SINK@ toggle";
        "XF86AudioMicMute" = "exec pactl set-source-mute @DEFAULT_SOURCE@ toggle";
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

      # Screen sharing fix
      exec systemctl --user import-environment XDG_SESSION_TYPE XDG_CURRENT_DESKTOP WAYLAND_DISPLAY SWAYSOCK
      exec hash dbus-update-activation-environment 2>/dev/null && dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP XDG_SESSION_TYPE

      include /etc/sway/config.d/*
    '';
  };
}
