{ compiler-nix-name ? "ghc922" }:
let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};
  # If ./nix/sources.nix file is not found run:
  #   niv init
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix { };
  # If haskellNix is not found run:
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  overlays = haskellNix.overlays ++ [
    (self: super: {
        libpng = super.libpng.overrideAttrs (_: { configureFlags = ["--enable-static"];});
        bucket = super.stdenv.mkDerivation {
            name = "bucket";
            src = super.lib.cleanSource ./go-src;
            buildInputs = [super.go];
        };
    })
  ];
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    sources.nixpkgs
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    (haskellNix.nixpkgsArgs // { inherit overlays; });
in pkgs.haskell-nix.cabalProject {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      src = ./.;
      name = "process-image";
    };
    index-state = "2022-04-12T00:00:00Z";
    index-sha256 = "9bf96168377dff50dcfbe4f9dbc5787a5059541644dee07e4992e1b21abd0bb9";
    plan-sha256 = if compiler-nix-name == "ghc922" then "0sbambmwp61g3sf21mmghgpcj83agzx4pqfvzqinsxj0f9fvdni6" else null;
    materialized = if compiler-nix-name == "ghc922" then ./nix/materialized else null;
    # Specify the GHC version to use.
    compiler-nix-name = compiler-nix-name;
  }
