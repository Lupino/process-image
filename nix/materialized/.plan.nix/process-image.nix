{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "process-image"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "MIT";
      maintainer = "lmjubuntu@gmail.com";
      author = "Li Meng Jun";
      homepage = "https://github.com/Lupino/process-image#readme";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."periodic-client" or (errorHandler.buildDepError "periodic-client"))
          (hsPkgs."periodic-common" or (errorHandler.buildDepError "periodic-common"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."hslogger" or (errorHandler.buildDepError "hslogger"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."JuicyPixels" or (errorHandler.buildDepError "JuicyPixels"))
          (hsPkgs."JuicyPixels-extra" or (errorHandler.buildDepError "JuicyPixels-extra"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."inline-c-cpp" or (errorHandler.buildDepError "inline-c-cpp"))
          ];
        libs = [ (pkgs."png" or (errorHandler.sysDepError "png")) ];
        buildable = true;
        modules = [
          "PI"
          "PI/GuetzliImage"
          "PI/ResizeImage"
          "PI/RemoveFile"
          "PI/SaveFile"
          "PI/Config"
          ];
        cxxSources = [
          "guetzli/guetzli/butteraugli_comparator.cc"
          "guetzli/guetzli/dct_double.cc"
          "guetzli/guetzli/debug_print.cc"
          "guetzli/guetzli/entropy_encode.cc"
          "guetzli/guetzli/fdct.cc"
          "guetzli/guetzli/gamma_correct.cc"
          "guetzli/guetzli/idct.cc"
          "guetzli/guetzli/jpeg_data.cc"
          "guetzli/guetzli/jpeg_data_decoder.cc"
          "guetzli/guetzli/jpeg_data_encoder.cc"
          "guetzli/guetzli/jpeg_data_reader.cc"
          "guetzli/guetzli/jpeg_data_writer.cc"
          "guetzli/guetzli/jpeg_huffman_decode.cc"
          "guetzli/guetzli/output_image.cc"
          "guetzli/guetzli/preprocess_downsample.cc"
          "guetzli/guetzli/processor.cc"
          "guetzli/guetzli/quality.cc"
          "guetzli/guetzli/quantize.cc"
          "guetzli/guetzli/score.cc"
          "guetzli/third_party/butteraugli/butteraugli/butteraugli.cc"
          "cxx-src/guetzli.cc"
          ];
        hsSourceDirs = [ "src" ];
        includeDirs = [ "guetzli" "guetzli/third_party/butteraugli" "cxx-src" ];
        };
      exes = {
        "process-image" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."process-image" or (errorHandler.buildDepError "process-image"))
            (hsPkgs."periodic-client" or (errorHandler.buildDepError "periodic-client"))
            (hsPkgs."periodic-common" or (errorHandler.buildDepError "periodic-common"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "process-image.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }