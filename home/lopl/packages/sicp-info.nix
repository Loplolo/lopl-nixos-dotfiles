{ stdenv, lib, fetchFromGitHub, texinfo }:

stdenv.mkDerivation {
  pname = "sicp-info";
  version = "master";

  src = fetchFromGitHub {
    owner = "pwiecz";
    repo = "sicp.texi";
    rev = "master";
    hash = lib.fakeHash; 
  };

  nativeBuildInputs = [ texinfo ];

  buildPhase = ''
    makeinfo sicp.texi
  '';

  installPhase = ''
    mkdir -p $out/share/info
    cp sicp.info* $out/share/info/
  '';
}
