{
  inputs,
  pkgs,
  ...
}: {
  imports = [inputs.vintagestory-nix.nixosModules.default];

  services.vintagestory = {
    enable = true;
    package = pkgs.vintagestoryPackages.latest;
    openFirewall = true;
    # host = 127.0.0.1
    # port = 42420
    # dataPath = vintagestory
  };
}
