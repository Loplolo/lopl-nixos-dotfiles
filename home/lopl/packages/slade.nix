{
  lib,
  appimageTools,
  fetchurl,
}: let
  version = "3.3.0-beta2";
  pname = "SLADE";

  src = fetchurl {
    url = "https://github.com/sirjuddington/${pname}/releases/download/${version}/${pname}.AppImage";

    sha256 = "sha256-6yCyuhqbdCOUe9m/BViQhISR2V/cPchkk5T4JkfBOBI=";
  };
in
  appimageTools.wrapType2 rec {
    inherit pname version src;
  }
