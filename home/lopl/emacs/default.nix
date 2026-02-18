{
  pkgs,
  lib,
  ...
}: let
  ampl-mode = pkgs.emacsPackages.trivialBuild {
    pname = "ampl-mode";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "dpo";
      repo = "ampl-mode";
      rev = "master";
      sha256 = "18ck61a98d86130snym5ykbi5hnn18nikqzvhwwg2w3iv1fd16lv";
    };
    preBuild = ''
      mv emacs/*.el .
    '';
  };

  quakec-mode = pkgs.emacsPackages.trivialBuild {
    pname = "quakec-mode";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "vkazanov";
      repo = "quakec-mode";
      rev = "master";
      sha256 = "0xb43s4641xxfbj6ybssp7aj09apw47qz2wlabv12wmsyf63db1x";
    };
  };

  helm-swoop = pkgs.emacsPackages.trivialBuild {
    pname = "helm-swoop";
    version = "master";
    src = pkgs.fetchFromGitHub {
      owner = "emacsattic";
      repo = "helm-swoop";
      rev = "master";
      sha256 = "01nrak72inmic9n30dval6608cfzsbv5izwzykbim46ifjhcipag";
    };
    packageRequires = [pkgs.emacsPackages.helm];
  };

in {
  home.file.".emacs.d/logo.png".source = ./logo.png;

  services.emacs.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;

    extraPackages = epkgs:
      with epkgs; [
        # System Integration
        envrc
        exec-path-from-shell
        
        # UI
        doom-modeline
        doom-themes
        all-the-icons
        dashboard
        page-break-lines
        visual-fill-column
        rainbow-delimiters
        which-key
        command-log-mode
        general
        hydra
        
        # EAF
        (eaf.withApplications [ eaf-browser eaf-pdf-viewer ])

        # Project Navigation
        helm
        helm-tramp
        helm-descbinds
        helm-projectile
        helm-lsp
        helm-swoop
        projectile
        treemacs
        treemacs-projectile
        treemacs-magit
        treemacs-all-the-icons

        # Tools
        magit
        all-the-icons-dired
        dired-open
        ibuffer-project
        pdf-tools
        vterm
        vterm-toggle
        eshell-vterm
        quickrun
        editorconfig
        
        # Completion
        company
        yasnippet
        yasnippet-snippets
        auto-complete

        # General Programming 
        flycheck
        ligature
        lsp-mode
        lsp-ui
        lsp-treemacs
        dap-mode
        treesit-auto
        (treesit-grammars.with-all-grammars)

        # Nix
        nix-mode
        
        # Python
        lsp-pyright
        python-black
        
        # Java
        lsp-java
        
        # Rust
        rustic
        cargo-mode
        cargo-transient
        
        # Web/Godot/C++
        gdscript-mode
        ccls
        web-mode
        impatient-mode
        
        # Scheme/Guile
        ac-geiser
        geiser
        geiser-guile
        
        # Latex/Org
        auctex
        org-bullets
        
        # Custom Builds
        ampl-mode
        quakec-mode

        # Typst
        typst-ts-mode
        typst-preview

        # Coq
        proof-general
        company-coq
        
        helpful                  
        eterm-256color           
        base16-theme   
        ement
      ];
    
    extraConfig = builtins.readFile ./init.el;
  };

  home.packages = with pkgs; [
    # System
    wmctrl
    xdotool
    ripgrep
    fd
    feh
    mpv

    # Fonts
    fira-code
    noto-fonts
    emacs-all-the-icons-fonts

    # Nix
    nil
    direnv 

    # Python tools
    pyright               
    python3Packages.black 
    python3Packages.isort 
    python3Packages.debugpy 
    python3

    # Latex/Typst
    auctex
    texlive.combined.scheme-full
    tinymist
    websocat

    # Programming languages
    cargo
    rustc
    rust-analyzer
    rustfmt
    clippy
    clang-tools
    jdt-language-server
    jdk17
    nodejs
    gnumake
    cmake
    gcc
  ];

  home.sessionVariables = {
    JAVA_HOME = "${pkgs.jdk17}";
  };
}
