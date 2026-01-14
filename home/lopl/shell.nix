{
  pkgs,
  osConfig,
  ...
}: {
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;

    oh-my-zsh = {
      enable = true;
      plugins = ["git" "sudo" "docker" "systemd"];
    };

    initContent = ''
      bat "${./.duck}" --style=plain --paging=never --color=always

       lslib() {
         local path
         path=$(nix-build '<nixpkgs>' -A "$1" --no-out-link)
         ls -la "$path/include"
       }
    '';

    shellAliases = {
      rebuild = "sudo nixos-rebuild switch --flake .#${osConfig.networking.hostName}";
      cleanup = "nix-collect-garbage -d";

      grep = "rg";
      cat = "bat";
      emc = "emacsclient -t";
      ".." = "cd ..";
      "..." = "cd ../..";
    };
  };

  programs.starship = {
    enable = true;
    settings = {
      format = "$username$hostname$directory $character$nix_shell$nodejs$lua$golang$rust$php$git_branch$git_commit$git_state$git_status";

      directory = {
        truncation_length = 0;
        truncate_to_repo = true;
        format = "$directory";
      };

      username = {
        show_always = true;
        format = "$user";
      };
      hostname = {
        ssh_only = false;
        format = "@$hostname:";
      };
      add_newline = false;
      character = {
        success_symbol = "λ";
        error_symbol = "λ";
      };
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.packages = with pkgs; [
    eza
    bat
    lolcat
    ripgrep
    fd
    fzf
    tldr
    lazygit
  ];
}
