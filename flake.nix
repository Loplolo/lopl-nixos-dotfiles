{
  description = "lopl's nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
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
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nix-flatpak,
    stylix,
    disko,
    alejandra,
    ...
  } @ inputs: let
    system = "x86_64-linux";

  in {
    nixosConfigurations = {
      rachael = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit inputs;};
        modules = [
 
          disko.nixosModules.disko
          stylix.nixosModules.stylix
          nix-flatpak.nixosModules.nix-flatpak
          ./systems/rachael/default.nix
          ./systems/rachael/disko.nix
          {
            environment.systemPackages = [alejandra.defaultPackage.${system}];
          }
          home-manager.nixosModules.home-manager
          {
            home-manager.extraSpecialArgs = {inherit inputs;};
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "backup";
            home-manager.users.lopl = import ./home/lopl/default.nix;
          }
        ];
      };

      roy = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {inherit inputs;};
        modules = [

          disko.nixosModules.disko
          stylix.nixosModules.stylix
          nix-flatpak.nixosModules.nix-flatpak
          ./systems/roy/default.nix
          ./systems/roy/disko.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.extraSpecialArgs = {inherit inputs;};
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "backup";
            home-manager.users.lopl = import ./home/lopl/default.nix;
          }
        ];
      };
    };
  };
}
