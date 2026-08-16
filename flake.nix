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

    alejandra.url = "github:kamadorueda/alejandra/4.0.0";
    alejandra.inputs.nixpkgs.follows = "nixpkgs";

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
    alejandra,
    sops-nix,
    vintagestory-nix,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    pkgs-stable = import nixpkgs-stable {
      inherit system;
      config.allowUnfree = true;
    };
  in {
    nixosConfigurations = {
      rachael = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {
          inherit inputs;
          inherit pkgs-stable;
        };
        modules = [
          disko.nixosModules.disko
          sops-nix.nixosModules.sops
          stylix.nixosModules.stylix
          nix-flatpak.nixosModules.nix-flatpak
          vintagestory-nix.nixosModules.default
          ./systems/rachael/default.nix
          ./systems/rachael/disko.nix
          {
            environment.systemPackages = [alejandra.defaultPackage.${system}];
          }
          home-manager.nixosModules.home-manager
          {
            home-manager.extraSpecialArgs = {
              inherit inputs;
              inherit pkgs-stable;
            };
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hm-bak";
            home-manager.users.lopl = import ./home/lopl/default.nix;
          }
        ];
      };

      pris = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {
          inherit inputs;
          inherit pkgs-stable;
        };
        modules = [
          disko.nixosModules.disko
          sops-nix.nixosModules.sops
          stylix.nixosModules.stylix
          nix-flatpak.nixosModules.nix-flatpak
          vintagestory-nix.nixosModules.default
          ./systems/pris/default.nix
          ./systems/pris/disko.nix
          {
            environment.systemPackages = [alejandra.defaultPackage.${system}];
          }
          home-manager.nixosModules.home-manager
          {
            home-manager.extraSpecialArgs = {
              inherit inputs;
              inherit pkgs-stable;
            };
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hm-bak";
            home-manager.users.lopl = import ./home/lopl/default.nix;
          }
        ];
      };

      roy = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {
          inherit inputs;
          inherit pkgs-stable;
        };
        modules = [
          disko.nixosModules.disko
          sops-nix.nixosModules.sops
          stylix.nixosModules.stylix
          nix-flatpak.nixosModules.nix-flatpak
          ./systems/roy/default.nix
          ./systems/roy/disko.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.extraSpecialArgs = {
              inherit inputs;
              inherit pkgs-stable;
            };
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "backup";
            home-manager.users.lopl = import ./home/lopl/default.nix;
          }
        ];
      };

      leon = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {
          inherit inputs;
          inherit pkgs-stable;
        };
        modules = [
          disko.nixosModules.disko
          sops-nix.nixosModules.sops
          vintagestory-nix.nixosModules.default
          ./systems/leon/default.nix
          ./systems/leon/disko.nix
          {
            environment.systemPackages = [alejandra.defaultPackage.${system}];
          }
        ];
      };
    };
  };
}
