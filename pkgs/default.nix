final: prev: {
  fteqw-latest = final.callPackage ./fteqw.nix {};
  nix-pills-info = final.callPackage ./nix-pills-info.nix {};
  paktool = final.callPackage ./paktool.nix {};
  qss-m = final.callPackage ./qss-m.nix {};
  sicp-info = final.callPackage ./sicp-info.nix {};
  slade = final.callPackage ./slade.nix {};
  trenchbroom-appimage = final.callPackage ./trenchbroom.nix {};
}
