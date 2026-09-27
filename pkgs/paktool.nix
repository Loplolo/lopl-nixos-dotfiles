{
  lib,
  fetchFromGitHub,
  fetchzip,
  pkgs,
}: let
  zlib-src = fetchzip {
    url = "https://github.com/madler/zlib/archive/refs/tags/v1.3.1.zip";
    sha256 = "sha256-TkPLWSN5QcPlL9D0kc/yhH0/puE9bFND24aj5NVDKYs=";
  };
in
  pkgs.stdenv.mkDerivation rec {
    pname = "paktool";
    version = "unstable-2024-10-16";

    src = pkgs.fetchFromGitHub {
      owner = "skalleAnka";
      repo = "paktool";
      rev = "2fac775639484c86266d79d746e6ad6838a1b5d3";
      sha256 = "0zc8axwi0jr13k4bay8k55hwhvk890fjjbkw706lpjg3f8n0dpdw";
    };

    nativeBuildInputs = [
      pkgs.cmake
    ];

    buildInputs = [
      pkgs.boost
    ];

    postPatch = ''
      mkdir -p build/_deps/zlib-src
      cp -r ${zlib-src}/* build/_deps/zlib-src/
      chmod -R +w build/_deps/zlib-src
    '';

    configurePhase = ''
            runHook preConfigure
            cmake -DCMAKE_BUILD_TYPE=Release \
                  -DFETCHCONTENT_FULLY_DISCONNECTED=ON \
                  -DBoost_NO_SYSTEM_PATHS=OFF \
      -DCMAKE_INSTALL_PREFIX=$out \
                  -B build
            runHook postConfigure
    '';

    buildPhase = ''
      runHook preBuild
      cmake --build build
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      cmake --install build --prefix "$out"
      runHook postInstall
    '';
  }
