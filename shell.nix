{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, exceptions, hspec, pcre-utils
  , QuickCheck, regex-pcre-builtin, stdenv, text
  }:
      mkDerivation {
        pname = "countable-inflections";
        version = "0.2.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring exceptions pcre-utils regex-pcre-builtin text
        ];
        testHaskellDepends = [ base hspec QuickCheck text ];
        homepage = "https://github.com/tippenein/countable-inflections";
        description = "Countable Text Inflections";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in
  if pkgs.lib.inNixShell then drv.env else drv
