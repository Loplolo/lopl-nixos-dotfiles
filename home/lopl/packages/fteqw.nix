{
  lib,
  fetchFromGitHub,
  stdenv,
  libopus,
  libxrandr,
  pname,
  releaseFile ? pname,
  buildFlags,
  buildInputs,
  nativeBuildInputs ? [],
  postFixup ? "",
  description,
  ...
}:
stdenv.mkDerivation {
  inherit
    pname
    buildFlags
    buildInputs
    nativeBuildInputs
    postFixup
    ;
  version = "2025-09-27";

  src = fetchFromGitHub {
    owner = "fte-team";
    repo = "fteqw";
    rev = "c781d13255ce72fcd24e47567244f970e1ba7c50";
    hash = lib.fakeHash;
  };

  makeFlags = [
    "PKGCONFIG=$(PKG_CONFIG)"
    "-C"
    "engine"
  ]; 

  enableParallelBuilding = true;
  postPatch = ''
    substituteInPlace ./engine/Makefile \
      --replace "I/usr/include/opus" "I${libopus.dev}/include/opus"
    substituteInPlace ./engine/gl/gl_vidlinuxglx.c \
      --replace 'Sys_LoadLibrary("libXrandr"' 'Sys_LoadLibrary("${libxrandr}/lib/libXrandr.so"'
  '';

  installPhase = ''
    runHook preInstall

    install -Dm755 engine/release/${releaseFile} $out/bin/${pname}

    runHook postInstall
  '';

  meta = {
    inherit description;
    homepage = "https://fteqw.org";
    longDescription = ''
      FTE is a game engine baed on QuakeWorld able to
      play games such as Quake 1, 2, 3, and Hexen 2.
      It includes various features such as extended map
      limits, vulkan and OpenGL renderers, a dedicated
      server, and fteqcc, for easier QuakeC development
    '';
    maintainers = with lib.maintainers; [necrophcodr lopl];
    license = lib.licenses.gpl2Plus;
    platforms = lib.platforms.linux;
  };
}
