{
  pkgs,
  osConfig,
  config,
  ...
}: {
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    dotDir = "${config.xdg.configHome}/zsh";
    antidote = {
      enable = true;
      plugins = [
        "zsh-users/zsh-autosuggestions"
        "ohmyzsh/ohmyzsh path:plugins/git"
        "ohmyzsh/ohmyzsh path:plugins/sudo"
        "ohmyzsh/ohmyzsh path:plugins/systemd"
      ];
    };
    initContent = ''
      bat "${./.duck}" --style=plain --paging=never --color=always
    '';

    shellAliases = {
      rebuild = "nixos-rebuild switch --flake ~/dotfiles/.#${osConfig.networking.hostName} --sudo";
      remote-rebuild = "nixos-rebuild switch --flake ~/dotfiles/.#leon --target-host lopl@leon --sudo --ask-sudo-password";
      cleanup = "nix-collect-garbage -d";
      update = "nix flake update";
      emc = "emacsclient -t";
      curl = "curl -4";
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
    nvd
    bat
    lolcat
    ripgrep
    fd
    fzf
    tldr
  ];
}
