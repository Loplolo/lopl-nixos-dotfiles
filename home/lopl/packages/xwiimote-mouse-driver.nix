{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  pname = "xwiimote-mouse-driver";
  version = "unstable-2026-07-31"; 

  src = pkgs.fetchFromGitHub {
    owner = "dvdhrm"; 
    repo = "xwiimote-mouse-driver";
    rev = "main";     
    sha256 = pkgs.lib.fakeSha256; 
  };

  nativeBuildInputs = [
    pkgs.cmake
    pkgs.pkg-config
    pkgs.gcc
  ];

  buildInputs = [
    pkgs.xwiimote
    pkgs.libevdev
    pkgs.systemd.dev
  ];

  postConfigure = ''
    mkdir -p build/sockpp-install/lib
  '';

  preBuild = ''
    if [ -d "build/sockpp-prefix/src/sockpp-build" ]; then
      ln -sf ../lib64/libsockpp.a build/sockpp-install/lib/libsockpp.a || true
    fi
  '';

  installPhase = ''
    install -Dm755 xwiimote-mouse-driver $out/bin/xwiimote-mouse-driver
  '';
}
