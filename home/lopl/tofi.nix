{pkgs, ...}: {
  programs.tofi = {
    enable = true;

    settings = {
      #font = "Fira Code";
      #font-size = 14;
      hint-font = true;

      #text-color = "#FFFFFF";

      #prompt-background = "#00000000";
      prompt-background-padding = 0;
      prompt-background-corner-radius = 0;

      #placeholder-color = "#FFFFFFA8";
      #placeholder-background = "#00000000";
      placeholder-background-padding = 0;
      placeholder-background-corner-radius = 0;

      #input-background = "#00000000";
      input-background-padding = 0;
      input-background-corner-radius = 0;

      #default-result-background = "#00000000";
      default-result-background-padding = 0;
      default-result-background-corner-radius = 0;

      #selection-color = "#A03232";
      #selection-background = "#00000000";
      selection-background-padding = 0;
      selection-background-corner-radius = 0;
      #selection-match-color = "#00000000";

      text-cursor-style = "bar";
      text-cursor-corner-radius = 0;

      prompt-text = " run: ";
      prompt-padding = 0;
      num-results = 0;
      result-spacing = 15;
      horizontal = true;
      min-input-width = 120;

      width = "100%";
      height = 30;
      #background-color = "#000000";

      outline-width = 0;
      #outline-color = "#080800";

      border-width = 0;
      #border-color = "#000000";

      corner-radius = 0;

      padding-top = 0;
      padding-bottom = 0;
      padding-left = 0;
      padding-right = 0;

      clip-to-padding = true;
      scale = true;

      anchor = "top";
      exclusive-zone = -1;

      margin-top = 1;
      margin-bottom = 0;
      margin-left = 0;
      margin-right = 0;
      hide-cursor = false;
      text-cursor = false;
      require-match = false;
      history = true;
      terminal = "alacritty";
      auto-accept-single = false;
      hide-input = false;
      hidden-character = "*";
      drun-launch = true;
      late-keyboard-init = false;
      multi-instance = false;
      ascii-input = false;
    };
  };

  home.packages = [pkgs.tofi];
}
