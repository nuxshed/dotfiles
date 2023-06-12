{
  description = "nuxsh's nixos configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";
    neovim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { nixpkgs, home-manager, nixpkgs-f2k, neovim-nightly, emacs-overlay, ... }@inputs: {
    nixosConfigurations = {
      earth = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/earth/configuration.nix
         {
           nixpkgs.overlays = [
            #nixpkgs-f2k.overlays.window-managers.awesome-git
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
      modules = [ ./hosts/earth/user.nix ];
    };
  };
  };
}
