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

    meta = {
      description = "It's a Doom editor";
      homepage = "https://slade.mancubus.net/";
      downloadPage = "https://github.com/sirjuddington/SLADE/releases";
      license = lib.licenses.gpl2;
      sourceProvenance = with lib.sourceTypes; [binaryNativeCode];
      maintainers = with lib.maintainers; [lopl];
      platforms = ["x86_64-linux"];
    };
  }
