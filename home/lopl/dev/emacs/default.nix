{
  pkgs,
  lib,
  ...
}: let
  ampl-mode = pkgs.emacsPackages.trivialBuild {
    pname = "ampl-mode";
    version = "unstable-2017-08-08";
    src = pkgs.fetchFromGitHub {
      owner = "dpo";
      repo = "ampl-mode";
      rev = "f9f996993adb467e014cdab92ed30aab0697f293";
      sha256 = "18ck61a98d86130snym5ykbi5hnn18nikqzvhwwg2w3iv1fd16lv";
    };
    preBuild = ''
      mv emacs/*.el .
    '';
  };

  quakec-mode = pkgs.emacsPackages.trivialBuild {
    pname = "quakec-mode";
    version = "unstable-2023-06-19";
    src = pkgs.fetchFromGitHub {
      owner = "vkazanov";
      repo = "quakec-mode";
      rev = "7b5d13fbdd9dfdc319ee8db1f1e954e00bdfce54";
      sha256 = "0xb43s4641xxfbj6ybssp7aj09apw47qz2wlabv12wmsyf63db1x";
    };
  };
in {
  home.file.".emacs.d/logo.png".source = ./logo.png;

  services.emacs.enable = true;
  systemd.user.services.emacs.Service.Environment = "COLORTERM=truecolor";
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs:
      with epkgs; [
        # System Integration
        envrc
        exec-path-from-shell

        # UI
        base16-theme
        doom-themes
        doom-modeline
        nerd-icons
        dashboard
        page-break-lines
        visual-fill-column
        rainbow-delimiters
        ligature

        # Help
        helpful
        command-log-mode
        general
        hydra

        # Editing
        smartparens
        apheleia
        avy
        goto-chg
        mwim
        crux
        move-text
        expreg
        iedit

        # Completion
        company
        company-box
        yasnippet
        yasnippet-snippets

        # Helm
        helm
        helm-tramp
        helm-descbinds
        helm-projectile
        helm-lsp

        # Files
        projectile
        magit
        treemacs
        treemacs-projectile
        treemacs-magit
        treemacs-nerd-icons
        nerd-icons-dired
        dired-open
        ibuffer-project

        # Terminal
        vterm
        vterm-toggle
        eshell-vterm
        eterm-256color
        quickrun

        # LSP & Debugging
        lsp-mode
        lsp-ui
        lsp-treemacs
        flycheck
        dap-mode

        # Tree-sitter
        treesit-auto
        (treesit-grammars.with-all-grammars)

        # Nix
        nix-ts-mode

        # Python
        lsp-pyright
        pyvenv
        python-pytest

        # Jupyter
        jupyter
        zmq
        websocket

        # R
        ess

        # Java
        lsp-java

        # Rust
        cargo-mode
        cargo-transient

        # C/C++
        cmake-mode

        # Web
        web-mode
        impatient-mode

        # Godot
        gdscript-mode

        # Scheme/Guile
        geiser
        geiser-guile

        # Typst
        typst-ts-mode
        typst-preview

        # AMPL
        ampl-mode

        # QuakeC
        quakec-mode

        # UML
        plantuml-mode

        # LaTeX
        auctex
        cdlatex
        xenops
        lsp-latex
        pdf-tools

        # Org
        org-superstar
        org-journal
        org-roam
        org-tree-slide
        ox-haunt

        # eBooks
        nov
      ];

    extraConfig = builtins.readFile ./init.el;
  };
}
