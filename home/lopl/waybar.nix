{pkgs, ...}: {
  programs.waybar = {
    enable = true;

    settings = {
      mainBar = {
        position = "bottom";
        height = 24;

        modules-left = ["sway/workspaces" "sway/mode" "sway/scratchpad" "custom/media"];
        modules-right = ["mpd" "idle_inhibitor" "temperature" "cpu" "memory" "network" "pulseaudio" "backlight" "keyboard-state" "battery" "battery#bat2" "clock" "tray"];
        # Sway mode
        "sway/mode" = {
          format = "<span style=\"italic\">{}</span>";
        };

        # Sway scratchpad
        "sway/scratchpad" = {
          format = "{icon} {count}";
          show-empty = false;
          format-icons = ["" ""];
          tooltip = true;
          tooltip-format = "{app}: {title}";
        };

        # MPD
        mpd = {
          format = "  {title} - {artist} {stateIcon} [{elapsedTime:%M:%S}/{totalTime:%M:%S}] {consumeIcon}{randomIcon}{repeatIcon}{singleIcon}[{songPosition}/{queueLength}] [{volume}%]";
          format-disconnected = " Disconnected";
          format-stopped = " {consumeIcon}{randomIcon}{repeatIcon}{singleIcon}Stopped";
          unknown-tag = "N/A";
          interval = 2;
          consume-icons = {
            on = " ";
          };
          random-icons = {
            on = " ";
          };
          repeat-icons = {
            on = " ";
          };
          single-icons = {
            on = "1 ";
          };
          state-icons = {
            paused = "";
            playing = "";
          };
          tooltip-format = "MPD (connected)";
          tooltip-format-disconnected = "MPD (disconnected)";
          on-click = "mpc toggle";
          on-click-right = "foot -a ncmpcpp ncmpcpp";
          on-scroll-up = "mpc volume +2";
          on-scroll-down = "mpc volume -2";
        };

        # Idle inhibitor
        idle_inhibitor = {
          format = "{icon}";
          format-icons = {
            activated = "";
            deactivated = "";
          };
        };

        # Tray
        tray = {
          icon-size = 21;
          spacing = 10;
        };

        # Clock
        clock = {
          tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
          format = "{:%a %d-%m-%Y %H:%M}";
        };

        # CPU
        cpu = {
          format = " {usage}%";
        };

        # Memory
        memory = {
          format = " {}%";
        };

        # Temperature
        temperature = {
          thermal-zone = 2;
          hwmon-path = "/sys/class/hwmon/hwmon1/temp1_input";
          critical-threshold = 80;
          format-critical = "{icon} {temperatureC}°C";
          format = "{icon} {temperatureC}°C";
          format-icons = ["" "" ""];
        };

        # Backlight
        backlight = {
          format = " {icon} {percent}%";
          format-icons = ["" ""];
        };

        # Battery
        battery = {
          states = {
            warning = 30;
            critical = 15;
          };
          format = " {icon} {capacity}%";
          format-charging = "  {capacity}%";
          format-plugged = "  {capacity}%";
          format-alt = "{icon} {time}";
          format-icons = ["" "" "" "" ""];
        };

        # Battery #2
        "battery#bat2" = {
          bat = "BAT2";
        };

        # Network
        network = {
          format-wifi = "{essid} ({signalStrength}%) ";
          format-ethernet = " {ifname}";
          tooltip-format = " {ifname} via {gwaddr}";
          format-linked = " {ifname} (No IP)";
          format-disconnected = "Disconnected ⚠️ {ifname}";
          format-alt = " {ifname}: {ipaddr}/{cidr}";
        };
        # PulseAudio
        pulseaudio = {
          scroll-step = 5;
          format = "{icon} {volume}% {format_source}";
          format-bluetooth = " {icon} {volume}% {format_source}";
          format-bluetooth-muted = "  {icon} {format_source}";
          format-muted = "  {format_source}";
          format-source = " {volume}%";
          format-source-muted = "";
          format-icons = {
            default = ["" "" ""];
          };
          on-click = "pavucontrol";
          on-click-right = "foot -a pw-top pw-top";
        };
      };
    };

    style = ''
      * {
          border: none;
          border-radius: 0;
          font-family: "Fira Code", "Font Awesome 6 Free";
          font-size: 18px;
          min-height: 0;
      }

      window#waybar {
          background: transparent;
      }

      #window {
          font-weight: bold;
          font-family: "Fira Code";
      }

      #workspaces button {
          padding: 0 5px;
          background: transparent;
          border-top: 2px solid transparent;
      }

      #workspaces button.focused {
          border-top: 2px solid #c9545d;
      }

      #mode {
          background: #64727D;
          border-bottom: 3px solid white;
      }

      #clock, #battery, #cpu, #memory, #network, #pulseaudio, #custom-spotify, #tray, #mode {
          padding: 0 3px;
          margin: 0 2px;
      }

      #clock {
          font-weight: bold;
      }

      #battery icon {
          color: red;
      }

      @keyframes blink {
          to {
              background-color: #ffffff;
              color: black;
          }
      }

      #battery.warning:not(.charging) {
          color: white;
          animation-name: blink;
          animation-duration: 0.5s;
          animation-timing-function: linear;
          animation-iteration-count: infinite;
          animation-direction: alternate;
      }

      #network.disconnected {
          background: #f53c3c;
      }

      #custom-spotify {
          color: rgb(102, 220, 105);
      }

      #tray {
          font-size: 20px;
      }
    '';
  };

  # Install necessary packages and fonts
  home.packages = with pkgs; [
    font-awesome_6
    playerctl
    mpc
    pavucontrol
  ];

  fonts.fontconfig.enable = true;
}
