{pkgs, ...}: {
  programs.nix-ld = {
    enable = true;
    libraries = with pkgs; [
      libGL
      libGLU
      libX11
      libXcursor
      libXrandr
      libXi
      alsa-lib
      stdenv.cc.cc.lib
      zlib
      openssl
    ];
  };
}
