{
  stdenv,
  lib,
  fetchurl
}:
stdenv.mkDerivation rec {
  pname = "nix-pills";
  version = "2021.0";

  src = fetchurl {
    url = "https://nixos.org/guides/nix-pills/nix-pills.epub";
    hash = lib.fakeHash;
  };

  dontUnpack = true;

  nativeBuildInputs = [ pandoc texinfo ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/info
    pandoc "$src" -f epub -t texinfo -o ${pname}.texi
    makeinfo ${pname}.texi -o $out/share/info/${pname}.info
    runHook postInstall
  '';
}
