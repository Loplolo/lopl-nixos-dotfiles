{
  pkgs,
  osConfig,
  config,
  lib,
  ...
}: let
  rebuild = pkgs.writeShellScriptBin "rebuild" ''
    cd ~/dotfiles || exit 1
    ${pkgs.alejandra}/bin/alejandra .
    git add -A

    if ! sudo nixos-rebuild switch --flake .#${osConfig.networking.hostName}; then
      exit 1
    fi

    gen_number=$(sudo nix-env --list-generations -p /nix/var/nix/profiles/system | grep current | awk '{print $1}')
    msg="[snapshot] host ${osConfig.networking.hostName}: gen $gen_number"

    # Check if the last commit was a snapshot
    last_msg=$(git log -1 --pretty=%s)

    if [[ "$last_msg" == "[snapshot]"* ]]; then
      git commit --amend -m "$msg"
    else
      git commit -m "$msg"
    fi
  '';

  remote-rebuild = pkgs.writeShellScriptBin "remote-rebuild" ''
      cd ~/dotfiles || exit 1
      git add -A

      if ! nixos-rebuild switch --flake .#leon \
        --target-host lopl@leon; then
        exit 1
      fi

      gen_number=$(ssh lopl@leon "nix-env --list-generations -p /nix/var/nix/profiles/system | grep current | awk '{print \$1}'")
      msg="[snapshot] host leon: gen $gen_number"

      last_msg=$(git log -1 --pretty=%s)
'';
in {
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

       lslib() {
         local path
         path=$(nix-build '<nixpkgs>' -A "$1" --no-out-link)
         ls -la "$path/include"
       }
    '';

    shellAliases = {
      cleanup = "nix-collect-garbage -d";
      update = "nix flake update";
      grep = "rg";
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
    rebuild
    remote-rebuild
    eza
    bat
    lolcat
    ripgrep
    fd
    fzf
    tldr
    lazygit
  ];

  home.sessionPath = [
    "$HOME/.config/guix/current/bin"
    "$HOME/.guix-profile/bin"
  ];

  home.sessionVariables = {
    GUIX_PROFILE = "$HOME/.guix-profile";
    GUIX_LOCPATH = "$HOME/.guix-profile/lib/locale";
  };
}
