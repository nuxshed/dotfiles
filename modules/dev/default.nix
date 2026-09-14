{ inputs, config, pkgs, lib, ... }: {
  home.packages = with pkgs; [
    inputs.quickshell-mcp.packages.${pkgs.stdenv.hostPlatform.system}.default
    bun
    cachix
    cloc
    docker-compose
    typst
    lua
    luarocks
    stylua
    direnv
    clojure
    clojure-lsp
    clj-kondo
    leiningen
    nodejs
    jre8
    nixfmt
    llvmPackages.bintools
    rustup
    racket
    python3
    gemini-cli
    claude-code
    (texlive.combine { inherit (texlive) scheme-full latexmk; })
    sqlite
  ];
  home.file.".config/clj-kondo/config.edn".text = ''
    {:ignore [:unresolved-symbol :unresolved-namespace :unused-value]}
  '';
}
