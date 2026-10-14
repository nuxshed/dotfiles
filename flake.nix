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
    helium = {
      url = "github:AlvaroParker/helium-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";
    spicetify-nix.url = "github:Gerg-L/spicetify-nix";
    quickshell = {
      url = "git+https://git.outfoxxed.me/outfoxxed/quickshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    quickshell-mcp = {
      url = "github:franklinnolasco7/quickshell-mcp";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    claude-desktop-extra = {
      url = "github:patrickjaja/claude-desktop-extra";
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
  };

  outputs = { nixpkgs, home-manager, nixpkgs-f2k, emacs-overlay, ... }@inputs:
    let
      overlays = [
        emacs-overlay.overlay
        nixpkgs-f2k.overlays.window-managers
        (self: super: {
          mpv = super.mpv.override { scripts = [ self.mpvScripts.mpris ]; };
        })
      ];

      # Home Manager runs as part of the system switch (one `nh os switch` does both).
      mkHost = host: { withHome ? true }: nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/${host}/configuration.nix
          { nixpkgs.overlays = overlays; }
        ] ++ nixpkgs.lib.optionals withHome [
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              backupFileExtension = "hm-bak";
              extraSpecialArgs = { inherit inputs; };
              users.nuxsh = import ./hosts/${host}/user.nix;
            };
          }
        ];
      };
    in {
      nixosConfigurations = {
        # earth's user.nix still targets HM 23.05 defaults (hyprlang, not our Lua config).
        earth = mkHost "earth" { withHome = false; };
        zephyrus = mkHost "zephyrus" { };
      };
    };
}
