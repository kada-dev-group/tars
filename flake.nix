{
  description = "A development shell for Haskell projects";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=5ae3b07d8d6527c42f17c876e404993199144b6a";
    flake-utils.url = "github:numtide/flake-utils";

  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        haskell-overlay = final: prev: {
          ghc-with-batteries = pkgs.haskell.packages.ghc910.ghcWithHoogle (
            hpkgs: with hpkgs; [
              haskell-language-server
              wai-app-static
            ]
          );
        };
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [ haskell-overlay ];
        };
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            cabal-install
            ghc-with-batteries
            ghcid
            fourmolu
          ];
          # Set up a shell hook for environment variables or commands to run on entry
          shellHook = ''
            echo "Entering Haskell development shell..."
            echo "GHC version: $(ghc --version)"
            echo "Cabal version: $(cabal --version)"
            echo "Haskell Language Server version: $(haskell-language-server --version)"
          '';
        };
        formatter = pkgs.nixpkgs-fmt;
      }
    );
}
