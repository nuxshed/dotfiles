{
  description = "nuxsh's nixos configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    zen-browser.url = "github:0xc000022070/zen-browser-flake";
    zen-browser.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";
    spicetify-nix.url = "github:Gerg-L/spicetify-nix";
    quickshell = {
      url = "git+https://git.outfoxxed.me/outfoxxed/quickshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bling = {
      url = "github:BlingCorp/bling";
      flake = false;
    };
    rubato = {
      url = "github:andOrlando/rubato";
      flake = false;
    };

    hyprland = {
      type = "git";
      url = "https://github.com/hyprwm/Hyprland";
      submodules = true;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, nixpkgs-f2k, emacs-overlay, spicetify-nix, quickshell, ... }@inputs: {
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
              [
                emacs-overlay.overlay
                nixpkgs-f2k.overlays.window-managers
                    (self: super: {
      mpv = super.mpv.override {
        scripts = [ self.mpvScripts.mpris ];
      };
    })
              ];
          }
        ];
      };
    };
  };
}
