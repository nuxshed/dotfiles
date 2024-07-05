{
  description = "nuxsh's nixos configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    spicetify-nix.url = "github:the-argus/spicetify-nix";
    
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";

    bling = { url = "github:BlingCorp/bling"; flake = false; };
    rubato = { url = "github:andOrlando/rubato"; flake = false; };
  };

  outputs = { nixpkgs, home-manager, nixpkgs-f2k, emacs-overlay, spicetify-nix, ... }@inputs: {
    nixosConfigurations = {
      earth = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/earth/configuration.nix
         {
           nixpkgs.overlays = [
             emacs-overlay.overlay
             nixpkgs-f2k.overlays.window-managers
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
          spicetify-nix.homeManagerModule
          {
            programs.spicetify = {
              enable = true;
              theme = spicetify-nix.packages.x86_64-linux.default.themes.catppuccin;
              colorScheme = "mocha";
              enabledExtensions = with spicetify-nix.packages.x86_64-linux.default.extensions; [
                fullAppDisplay
                shuffle
                adblock
                lastfm
                history
                keyboardShortcut
                bookmark
                powerBar
                phraseToPlaylist
              ];
            };
          }
         {
           nixpkgs.overlays = [
             emacs-overlay.overlay
             nixpkgs-f2k.overlays.window-managers
           ];
         }
      ];
    };
  };
  };
}
