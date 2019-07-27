{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, base64-bytestring, blaze-builder
      , blaze-markup, bytestring, classy-prelude, classy-prelude-conduit
      , classy-prelude-yesod, conduit, conduit-extra, containers
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
          classy-prelude classy-prelude-conduit classy-prelude-yesod conduit
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
          base classy-prelude classy-prelude-yesod hspec monad-logger
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
