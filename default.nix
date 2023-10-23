{ compiler-nix-name ? "ghc947" }:
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
    index-state = "2023-10-15T00:00:00Z";
    index-sha256 = "7f445a790f82e69f7453632d1d5eb993a9c6725fc4ef5d7e4a48fb89bd2c7dc6";
    sha256map = import ./nix/sha256map.nix;
    # Specify the GHC version to use.
    compiler-nix-name = compiler-nix-name;
  }
