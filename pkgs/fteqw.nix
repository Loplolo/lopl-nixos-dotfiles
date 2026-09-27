{
  lib,
  stdenv,
  fetchFromGitHub,
  pkg-config,
  makeWrapper,
  libopus,
  libvorbis,
  speex,
  libxrandr,
  libpng,
  libjpeg,
  alsa-lib,
  libGL,
  zlib,
  gnutls,
  libx11,
  libxext,
  libxscrnsaver,
  libxxf86vm,
  libxcursor,
  libxinerama,
  libxi,
}:
stdenv.mkDerivation rec {
  pname = "fteqw";
  version = "2025-09-27";

  src = fetchFromGitHub {
    owner = "fte-team";
    repo = "fteqw";
    rev = "c781d13255ce72fcd24e47567244f970e1ba7c50";
    sha256 = "sha256-C6v7lxuqsqX/Wo6oKi5yeD8QNYTg5wFxWo03qGszkQ8=";
  };

  nativeBuildInputs = [
    pkg-config
    makeWrapper
  ];

  buildInputs = [
    libopus
    libvorbis
    speex
    libxrandr
    libpng
    libjpeg
    alsa-lib
    libGL
    zlib
    gnutls
    libx11
    libxext
    libxscrnsaver
    libxxf86vm
    libxcursor
    libxinerama
    libxi
  ];

  hardeningDisable = ["fortify" "format"];

  buildFlags = ["gl-rel"];

  makeFlags = [
    "PKGCONFIG=$(PKG_CONFIG)"
    "-C"
    "engine"
  ];

  enableParallelBuilding = false;

  postPatch = ''
    substituteInPlace ./engine/Makefile \
      --replace "I/usr/include/opus" "I${libopus.dev}/include/opus"
    substituteInPlace ./engine/gl/gl_vidlinuxglx.c \
      --replace 'Sys_LoadLibrary("libXrandr"' 'Sys_LoadLibrary("${libxrandr}/lib/libXrandr.so"'
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    if [ -f engine/release/fteqw-gl ]; then
      cp engine/release/fteqw-gl $out/bin/${pname}
    else
      cp engine/release/fteqw* $out/bin/${pname}
    fi

    runHook postInstall
  '';

  postFixup = ''
    wrapProgram $out/bin/${pname} \
      --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath buildInputs}
  '';
}
