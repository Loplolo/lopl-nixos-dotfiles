{
  pkgs,
  lib,
  ...
}: {
  programs.vscode = {
    enable = true;

    extensions = with pkgs.vscode-extensions;
      [
        llvm-vs-code-extensions.vscode-clangd
        ms-vscode.cmake-tools
        twxs.cmake
        vscode-emacs.emacs
      ]
      ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "quakec";
          publisher = "joshuaskelly";
          version = "0.2.3";
          sha256 = lib.fakeSha256; # See instructions below on how to get the real hash
        }
        # Guile / Scheme LSP
        {
          name = "scheme-lsp";
          publisher = "rgherdt";
          version = "0.1.7"; # Replace with the current version if it updates
          sha256 = lib.fakeSha256;
        }
        # Makefile Tools (often needs to be pulled from the marketplace)
        {
          name = "makefile-tools";
          publisher = "ms-vscode";
          version = "0.8.22";
          sha256 = lib.fakeSha256;
        }
      ];

    # 3. VSCode settings to bind the extensions to the Nix binaries
    userSettings = {
      # Emacs extension recommendation to prevent shortcut conflicts
      "keyboard.dispatch" = "keyCode";

      # Force VSCode to use the Nix-provided clangd binary
      "clangd.path" = "${pkgs.clang-tools}/bin/clangd";

      # Scheme LSP settings
      "scheme.lsp.serverPath" = "scheme-lsp-server";
    };
  };
}
