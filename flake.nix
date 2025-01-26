{
  description = "nuxsh's nixos configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";
    bling = {
      url = "github:BlingCorp/bling";
      flake = false;
    };
    rubato = {
      url = "github:andOrlando/rubato";
      flake = false;
    };
  };

  outputs = { nixpkgs, home-manager, nixpkgs-f2k, emacs-overlay, ... }@inputs: {
    nixosConfigurations = {
      earth = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/earth/configuration.nix
          {
            nixpkgs.overlays =
              [ emacs-overlay.overlay nixpkgs-f2k.overlays.window-managers ];
          }
        ];
      };
      zephyrus = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/zephyrus/configuration.nix
          {
            nixpkgs.overlays =
              [ emacs-overlay.overlay nixpkgs-f2k.overlays.window-managers ];
          }
        ];
      };
    };

    homeConfigurations = {
      "zephyrus" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        extraSpecialArgs = { inherit inputs; };
        modules = [
          ./hosts/zephyrus/user.nix
          {
            nixpkgs.overlays =
              [ emacs-overlay.overlay nixpkgs-f2k.overlays.window-managers ];
          }
        ];
      };
    };
  };
}
