{
  description = "nuxsh's nixos configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    
    neovim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { nixpkgs, home-manager, neovim-nightly, emacs-overlay, ... }@inputs: {
    nixosConfigurations = {
      earth = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/earth/configuration.nix
         {
           nixpkgs.overlays = [
             neovim-nightly.overlay
             emacs-overlay.overlay
           ];
         }
        ];
      };
    };

  homeConfigurations = {
    "earth" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      extraSpecialArgs = { inherit inputs; };
      modules = [
        ./hosts/earth/user.nix
         {
           nixpkgs.overlays = [
             neovim-nightly.overlay
             emacs-overlay.overlay
           ];
         }
      ];
    };
  };
  };
}
