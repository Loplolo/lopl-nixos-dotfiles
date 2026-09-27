{
  stdenv,
  lib,
  fetchurl,
  gzip,
}:
stdenv.mkDerivation rec {
  pname = "sicp-info";
  version = "2021.0";

  src = fetchurl {
    url = "http://www.neilvandyke.org/sicp-texi/sicp.info.gz";
    hash = "sha256-erfHjk1M3xaJ6wxo+i58kuUejWav8WiJLYgSsT4rC2M=";
  };

  dontUnpack = true;

  nativeBuildInputs = [gzip];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/info
    gzip -d -c $src > $out/share/info/sicp.info

    runHook postInstall
  '';
}
