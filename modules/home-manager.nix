{
  inputs,
  pkgs-stable,
  ...
}: {
  home-manager = {
    extraSpecialArgs = {inherit inputs pkgs-stable;};
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "hm-bak";
    users.lopl = import ../home/lopl;
  };
}
