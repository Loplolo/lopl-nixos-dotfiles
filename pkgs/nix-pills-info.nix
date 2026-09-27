{
  stdenv,
  lib,
  fetchurl,
  pandoc,
  texinfo,
}:
stdenv.mkDerivation rec {
  pname = "nix-pills";
  version = "2021.0";

  src = fetchurl {
    url = "https://nixos.org/guides/nix-pills/nix-pills.epub";
    hash = "sha256-2qV5/8jeDt/Je9CxwM4PIx6PtdwzUA+wkQDSBFhzRWE=";
  };

  dontUnpack = true;

  nativeBuildInputs = [pandoc texinfo];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/info

    pandoc "$src" -f epub -t texinfo --standalone -o body.texi

    cat << 'EOF' > ${pname}.texi
    @dircategory Nix
    @direntry
    * Nix Pills: (nix-pills).      The NixOS pills guide.
    @end direntry
    EOF

    cat body.texi >> ${pname}.texi

    makeinfo --force ${pname}.texi -o $out/share/info/${pname}.info

    runHook postInstall
  '';
}
