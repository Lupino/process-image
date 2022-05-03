{ compiler-nix-name ? "ghc922", crossPlatforms ? ps: with ps; [] }:
let
  project = import ./default.nix { compiler-nix-name = compiler-nix-name; };
in
  project.shellFor {
    packages = ps: with ps; [ ];
    withHoogle = false;

    tools = {};
    buildInputs = [];
    crossPlatforms = crossPlatforms;
    exactDeps = false;
  }
