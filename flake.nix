{
  description = "My NixOS Configuration";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    neovim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";

    # awesomewm modules
    bling = { url = "github:BlingCorp/bling"; flake = false; };
    rubato = { url = "github:andOrlando/rubato"; flake = false; };
  };

  outputs = { self, nixpkgs, home-manager, neovim-nightly, emacs-overlay, nixpkgs-f2k, ... }@inputs:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; }; # Forgive me Mr. Stallman
      };

      lib = nixpkgs.lib.extend
        (final: prev:
          let
            inherit (lib) mkOption types;
          in
          {

            mkOpt = type: default:
              mkOption { inherit type default; };

            mkOpt' = type: default: description:
              mkOption { inherit type default description; };

            mkBoolOpt = default: mkOption {
              inherit default;
              type = types.bool;
              example = true;
            };
          });


      extraSpecialArgs = {
        inherit inputs self;
        bling = inputs.bling;
        rubato = inputs.rubato;
      };

      overlays = [
        nixpkgs-f2k.overlay
        neovim-nightly.overlay
        emacs-overlay.overlay
      ];
    in
    {
      homemanagerConfigurations = {
        earth = home-manager.lib.homeManagerConfiguration {
          inherit extraSpecialArgs;
          configuration = { pkgs, config, ... }:
            {
              home.stateVersion = "21.11";
              programs.home-manager.enable = true;
              nixpkgs.overlays = overlays;
              imports = [ ./hosts/earth/user.nix ];
            };
          system = "x86_64-linux";
          homeDirectory = "/home/advait";
          username = "advait";
          stateVersion = "21.11";
        };
      };
      nixosConfigurations = {
        earth = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            {
              nixpkgs.overlays = overlays;
            }
            ./hosts/earth/configuration.nix
          ];
        };
      };
    };
}
