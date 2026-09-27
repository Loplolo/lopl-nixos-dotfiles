{
  description = "lopl's nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.11";
    nix-flatpak.url = "github:gmodena/nix-flatpak";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:nix-community/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    firefox-native-base16.url = "github:GnRlLeclerc/firefox-native-base16";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    vintagestory-nix.url = "git+https://codeberg.org/PierreBorine/vintagestory-nix";
    vintagestory-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-stable,
    home-manager,
    nix-flatpak,
    stylix,
    disko,
    sops-nix,
    vintagestory-nix,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    pkgs-stable = import nixpkgs-stable {
      inherit system;
      config.allowUnfree = true;
    };

    mkHost = name: extraModules:
      nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit inputs pkgs-stable;};
        modules =
          [
            disko.nixosModules.disko
            sops-nix.nixosModules.sops
            ./systems/${name}
            ./systems/${name}/disko.nix
          ]
          ++ extraModules;
      };

    desktopModules = [
      stylix.nixosModules.stylix
      nix-flatpak.nixosModules.nix-flatpak
      vintagestory-nix.nixosModules.default
      home-manager.nixosModules.home-manager
      ./modules/home-manager.nix
      ./modules/desktop
    ];
  in {
    formatter.${system} = nixpkgs.legacyPackages.${system}.alejandra;

    nixosConfigurations = {
      rachael = mkHost "rachael" desktopModules;
      pris = mkHost "pris" desktopModules;
      roy = mkHost "roy" desktopModules;
      leon = mkHost "leon" [./modules/server];
    };
  };
}
