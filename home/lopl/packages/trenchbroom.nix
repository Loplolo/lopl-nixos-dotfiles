{
  lib,
  appimageTools,
  fetchzip,
}: let
  version = "v2026.1";
  pname = "TrenchBroom";

  extracted = fetchzip {
    url = "https://github.com/TrenchBroom/${pname}/releases/download/${version}/${pname}-Linux-x86_64-${version}-Release.zip";
    sha256 = "sha256-BUNxtM7VJ4L6upg154RnGT4Dd4s1X+ZdJ0Tjov0tmrw=";
  };
  src = "${extracted}/${pname}.AppImage";
in
  appimageTools.wrapType2 rec {
    inherit pname version src;
  }
