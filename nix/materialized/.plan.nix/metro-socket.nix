{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "metro-socket"; version = "0.1.0.1"; };
      license = "BSD-3-Clause";
      copyright = "MIT";
      maintainer = "lmjubuntu@gmail.com";
      author = "Lupino";
      homepage = "https://github.com/Lupino/metro#readme";
      url = "";
      synopsis = "Socket transport for metro";
      description = "Please see the README on GitHub at <https://github.com/Lupino/metro/tree/master/metro-socket#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "ChangeLog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."hslogger" or (errorHandler.buildDepError "hslogger"))
          (hsPkgs."map-io" or (errorHandler.buildDepError "map-io"))
          (hsPkgs."metro" or (errorHandler.buildDepError "metro"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          ];
        buildable = true;
        modules = [
          "Paths_metro_socket"
          "Metro/Socket"
          "Metro/SocketServer"
          "Metro/TCPServer"
          "Metro/TP/Socket"
          "Metro/TP/TCPSocket"
          "Metro/TP/UDPSocket"
          "Metro/UDPServer"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "1";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "1";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/metro-socket; echo source root reset to $sourceRoot";
    }) // { cabal-generator = "hpack"; }