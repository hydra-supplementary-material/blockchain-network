{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-ledger-test"; version = "1.3.0"; };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Test helpers from cardano-ledger exposed to other packages";
      description = "Test helpers from cardano-ledger exposed to other packages";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.base16-bytestring)
          (hsPkgs.bimap)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-binary-test)
          (hsPkgs.cardano-ledger)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-crypto-test)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-prelude-test)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.cs-blockchain)
          (hsPkgs.cs-ledger)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.formatting)
          (hsPkgs.generic-monoid)
          (hsPkgs.hedgehog)
          (hsPkgs.lens)
          (hsPkgs.mtl)
          (hsPkgs.optparse-applicative)
          (hsPkgs.resourcet)
          (hsPkgs.small-steps)
          (hsPkgs.streaming)
          (hsPkgs.tasty)
          (hsPkgs.tasty-hedgehog)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.vector)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger";
      rev = "6920692cd4fa412c9ae0e9b82fa46c2d275623fb";
      sha256 = "1zzdzcvf6wbc0zcn6rls4kch8qvpks8mchlwawpjz62khbb1figw";
      });
    postUnpack = "sourceRoot+=/cardano-ledger/test; echo source root reset to \$sourceRoot";
    }