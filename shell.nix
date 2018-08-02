{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc843", doBenchmark ? false
}:

let

  inherit (nixpkgs) pkgs;

  cpyNek0 =
    with haskellPackages; callPackage (
    { mkDerivation, aeson, base, classy-prelude, classy-prelude-conduit
    , data-default, http-conduit, http-types, persistent, stdenv, yesod
    , yesod-newsfeed, yesod-static
    }:
    mkDerivation {
      pname = "classy-prelude-yesod";
      version = "1.4.0";
      sha256 = "0cd2a88f42c3541ba4bce6801c8b8d9c331e1c49a6288bf16f764676a34b9e28";
      libraryHaskellDepends = [
        aeson base classy-prelude classy-prelude-conduit data-default
        http-conduit http-types persistent yesod yesod-newsfeed
        yesod-static
      ];
      doHaddock = false;
      homepage = "https://github.com/snoyberg/mono-traversable#readme";
      description = "Provide a classy prelude including common Yesod functionality";
      license = stdenv.lib.licenses.mit;
    }) {};

  f = { mkDerivation, aeson, base, base64-bytestring, blaze-builder
      , blaze-markup, bytestring, classy-prelude, classy-prelude-conduit
      , conduit, conduit-extra, containers
      , cryptohash, data-default, directory, fast-logger, file-embed
      , HDBC, HDBC-postgresql, hjsmin, hspec, http-conduit, JuicyPixels
      , JuicyPixels-scale-dct, mime-mail, monad-control, monad-logger
      , persistent, persistent-mysql, persistent-postgresql
      , persistent-template, resourcet, safe, safe-exceptions
      , shakespeare, split, stdenv, template-haskell, text, time
      , transformers, unordered-containers, vector, wai, wai-extra
      , wai-logger, warp, yaml, yesod, yesod-auth, yesod-core, yesod-form
      , yesod-static, yesod-test
      }:
      mkDerivation {
        pname = "yammat";
        version = "0.0.8";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base base64-bytestring blaze-builder blaze-markup bytestring
          classy-prelude classy-prelude-conduit cpyNek0 conduit
          conduit-extra containers cryptohash data-default directory
          fast-logger file-embed HDBC HDBC-postgresql hjsmin http-conduit
          JuicyPixels JuicyPixels-scale-dct mime-mail monad-control
          monad-logger persistent persistent-postgresql persistent-template
          safe safe-exceptions shakespeare split template-haskell text time
          unordered-containers vector wai wai-extra wai-logger warp yaml
          yesod yesod-auth yesod-core yesod-form yesod-static
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [
          base classy-prelude cpyNek0 hspec monad-logger
          persistent persistent-mysql resourcet transformers yesod yesod-core
          yesod-test
        ];
        license = stdenv.lib.licenses.agpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
