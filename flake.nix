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
    layout-machi = { url = "github:xinhaoyuan/layout-machi"; flake = false; };
  };

  outputs = { self, nixpkgs, home-manager, neovim-nightly, emacs-overlay, nixpkgs-f2k, ... }@inputs:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; }; # Forgive me Mr. Stallman
      };

      extraSpecialArgs = {
        inherit inputs self;
        bling = inputs.bling;
        layout-machi = inputs.layout-machi;
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
              imports = [
                ./modules/shell
                ./modules/editors/neovim.nix
                ./modules/editors/emacs.nix
                ./modules/editors/vim.nix
                ./modules/dev/clojure.nix
                ./modules/dev/lua.nix
                ./modules/dev/node.nix
                ./modules/dev/rust.nix
                ./modules/dev/nix.nix
                ./modules/dev/python.nix
                ./modules/programs
                ./modules/programs/graphics.nix
                ./modules/programs/mail.nix
                ./modules/desktop
                ./modules/desktop/windowManagers/awesome.nix
                ./modules/desktop/windowManagers/berry.nix
                ./modules/desktop/windowManagers/herbstluftwm.nix
                ./modules/desktop/windowManagers/i3.nix
                ./modules/desktop/windowManagers/sway.nix
              ];
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
