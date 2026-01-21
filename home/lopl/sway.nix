{
  pkgs,
  lib,
  ...
}: let
  mod = "Mod1";
  sup = "Mod4";
  menu = "tofi-run | xargs swaymsg exec --";
  drun = "tofi-drun | xargs swaymsg exec --";
in {
  wayland.windowManager.sway = {
    enable = true;

    config = {
      modifier = mod;
      terminal = "alacritty";

      output = {
        "*" = {
          bg = "${./quake.png} fill";
        };
        "Virtual-1" = {
          mode = "1920x1080@60Hz";
        };
      };

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
        # Unbind default layout tabbed bind (annoying for emacs)
        "${mod}+w" = "null";

        # Basics
        "${mod}+Return" = "exec alacritty";
        "${mod}+Shift+q" = "kill";
        "${mod}+d" = "exec ${drun}";
        "${mod}+Shift+d" = "exec ${menu}";

        # Reload/Exit
        "${mod}+Shift+r" = "reload";
        "${mod}+Shift+e" = "exec swaynag -t warning -m 'You pressed the exit shortcut.' -B 'Yes, exit sway' 'swaymsg exit'";

        # Custom Apps
        "${sup}+f" = "exec nyxt --no-socket";
        "${sup}+g" = "exec firefox";
        "${sup}+t" = "exec Telegram";
        "${sup}+r" = "exec thunderbird";
        "${sup}+c" = "exec code";
        "${sup}+m" = "exec alacritty -e cmus";
        "${sup}+s" = "exec steam";
        "${sup}+e" = "exec emacsclient -c";
        "${sup}+q" = "exec ironwail -basedir ~/.q1/";
        "${sup}+w" = "exec wike";
        "${sup}+x" = "exec xournalpp";
        "${sup}+o" = "exec obsidian";

        # Screenshots
        "--release ${mod}+Shift+s" = "exec flameshot gui";

        # Audio
        "XF86AudioRaiseVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ +10%";
        "XF86AudioLowerVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ -10%";
        "XF86AudioMute" = "exec pactl set-sink-mute @DEFAULT_SINK@ toggle";
        "XF86AudioMicMute" = "exec pactl set-source-mute @DEFAULT_SOURCE@ toggle";
      };
    };

    extraConfig = ''
      # Passthrough mode
      mode "passthrough" {
          bindsym Pause mode default
          bindsym ${mod}+Escape mode "default"
      }
      bindsym ${mod}+Escape mode passthrough

    '';
  };
}
