{
  lib,
  stdenv,
  fetchFromGitHub,
  pkg-config,
  sdl3,
  libGL,
  gnutls,
  libmad,
  libopus,
  opusfile,
  libvorbis,
  zlib,
  curl,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "qss-m";
  version = "1.6.5-unstable-2026-10-12";

  src = fetchFromGitHub {
    owner = "timbergeron";
    repo = "QSS-M";
    rev = "21a5d106f977fa4f544eaa89a7400eba5cc29d89";
    hash = "sha256-I4EAn9ceJQy7wdrJWx+nrpAqd1nGrqJiQWzHaTeSlDg=";
  };

  sourceRoot = "${finalAttrs.src.name}/Quake";

  nativeBuildInputs = [pkg-config];

  buildInputs = [
    sdl3
    libGL
    gnutls
    libmad
    libopus
    opusfile
    libvorbis
    zlib
    curl
  ];

  env.NIX_CFLAGS_COMPILE = toString [
    "-I${lib.getDev libopus}/include/opus"
  ];

  makeFlags = [
    # Required on GCC >= 10 see BUILDING.html
    "QSS_LDFLAGS=-Wl,--allow-multiple-definition"
  ];

  enableParallelBuilding = true;
  installPhase = ''
    runHook preInstall
    install -Dm755 quakespasm $out/bin/qss-m
    runHook postInstall
  '';

  meta = {
    description = "QSS-M, a port of ID Software's original Quake, evolved from Quake 1.09 to Fitzquake, Quakespasm, Quakespasm Spiked, and now Quakespasm Spiked Multiplayer. NetQuake competitive multiplayer clients began with ProQuake, progressed to Qrack or Mark V, and arrived at QSS-M, thanks to voluntary contributions.";
    homepage = "https://github.com/timbergeron/QSS-M";
    license = lib.licenses.gpl2Plus;
    platforms = lib.platforms.linux;
    maintainers = with lib.maintainers; [lopl];
    mainProgram = "qss-m";
  };
})
