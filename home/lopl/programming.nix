{pkgs, ...}: {
  home.packages = with pkgs; [
    # Nix
    nil
    nixfmt

    # Python
    pyright
    ruff
    (python3.withPackages (
      ps:
        with ps; [
          debugpy
          pytest
          virtualenv
          pip
          ipython
          jupyter
          jupyter-client
          ipykernel
          pandas
          numpy
          matplotlib
        ]
    ))

    # Guile and Guix
    guile
    guile-commonmark
    guile-hall
    guile-hoot
    guile-lib
    guile-reader
    guix
    haunt

    # LaTeX
    texlab
    texliveFull
    ghostscript

    # Typst
    tinymist
    websocat

    # Rust
    cargo
    rustc
    rust-analyzer
    rustfmt
    clippy

    # C/C++
    gcc
    clang-tools
    gnumake
    cmake

    # Java
    jdk17
    jdt-language-server

    # JavaScript
    nodejs
  ];

  home.sessionVariables = {
    JAVA_HOME = "${pkgs.jdk17}";
  };
}
