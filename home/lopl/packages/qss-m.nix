{
  lib,
  stdenv,
  fetchFromGitHub,
  pkgs,
}:
stdenv.mkDerivation rec {
  pname = "qss-m";
  version = "unstable-2026-07-21";

  src = fetchFromGitHub {
    owner = "timbergeron";
    repo = "QSS-M";
    rev = "b7ef8b48e167497ee29333d26df0b598dbf0d876";
    sha256 = "1lbi9mw5by7swysyqbfd2sfhdikc18lah4pl44r39gyc999sgzvs";
  };

  nativeBuildInputs = with pkgs; [
    pkg-config
    gnumake
    zip
  ];

  buildInputs = with pkgs; [
    SDL2
    libGL
    gnutls
    libmad
    libopus
    opusfile
    libvorbis
    zlib
    curl
    xorg.libX11
    xorg.xorgproto
  ];

  sourceRoot = "${src.name}/Quake";

  # Correctly reference the dev outputs via the 'pkgs' set
  NIX_CFLAGS_COMPILE = [
    "-I${pkgs.libopus.dev}/include/opus"
    "-I${pkgs.opusfile.dev}/include/opus"
  ];

  makeFlags = [
    "DO_CODECS=1"
    "USE_SDL2=1"
    "QSS_LDFLAGS=-Wl,--allow-multiple-definition"
  ];

  buildPhase = ''
    runHook preBuild
    make $makeFlags -j$NIX_BUILD_CORES
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp quakespasm $out/bin/qss-m

    runHook postInstall
  '';

  meta = with lib; {
    description = "QSS-M, a port of ID Software's original Quake, evolved from Quake 1.09 to Fitzquake, Quakespasm, Quakespasm Spiked, and now Quakespasm Spiked Multiplayer. NetQuake competitive multiplayer clients began with ProQuake, progressed to Qrack or Mark V, and arrived at QSS-M, thanks to voluntary contributions";
    homepage = "qssm.quakeone.com";
    license = licenses.gpl2Plus;
    platforms = platforms.linux;
    maintainers = [lopl];
  };
}
