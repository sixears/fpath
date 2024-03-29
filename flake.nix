{
  description = "Strongly-typed file paths";

  inputs = {
    nixpkgs.url      = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    build-utils.url  = github:sixears/flake-build-utils/r1.0.0.13;

    base1t.url               = github:sixears/base1t/r0.0.5.36;
    exited.url               = github:sixears/exited/r1.0.4.23;
    has-callstack.url        = github:sixears/has-callstack/r1.0.1.19;
    monaderror-io.url        = github:sixears/monaderror-io/r1.2.5.20;
    more-unicode.url         = github:sixears/more-unicode/r0.0.17.12;
    non-empty-containers.url = github:sixears/non-empty-containers/r1.4.3.36;
    quasiquoting.url         = github:sixears/quasiquoting/r1.0.1.32;
    tasty-plus.url           = github:sixears/tasty-plus/r1.5.2.24;
    tfmt.url                 = github:sixears/tfmt/r0.2.7.25;
  };

  outputs = { self, nixpkgs, build-utils
            , base1t, exited, has-callstack, monaderror-io, more-unicode
            , non-empty-containers, quasiquoting, tasty-plus, tfmt
            }:
    build-utils.lib.hOutputs self nixpkgs "fpath" {
#      deps = {
#        inherit base1t exited has-callstack monaderror-io more-unicode
#                non-empty-containers quasiquoting tasty-plus tfmt;
#      };
      ghc = p: p.ghc8107; # for tfmt

      callPackage = { mkDerivation, lib, mapPkg, system
                    , base, base-unicode-symbols, containers, data-default
                    , data-textual, deepseq, directory, exceptions, filepath
                    , genvalidity, genvalidity-bytestring, genvalidity-property
                    , genvalidity-text, lens, mono-traversable, mtl
                    , optparse-applicative, parsers, QuickCheck, safe, tasty
                    , tasty-hunit, tasty-quickcheck, template-haskell, temporary
                    , text, text-printer, th-lift-instances, unix, validity }:
        let
          pkg = build-utils.lib.flake-def-pkg system;
        in
          mkDerivation {
            pname = "fpath";
            version = "1.3.2.39";
            src = ./.;
            libraryHaskellDepends = [
              base base-unicode-symbols containers data-default data-textual
              deepseq directory exceptions filepath genvalidity
              genvalidity-bytestring genvalidity-property genvalidity-text
              lens mono-traversable mtl optparse-applicative parsers QuickCheck
              safe tasty tasty-hunit tasty-quickcheck template-haskell temporary
              text text-printer th-lift-instances unix validity
            ] ++ map (p: pkg p) [
              base1t exited has-callstack monaderror-io more-unicode
              non-empty-containers quasiquoting tasty-plus tfmt
            ];
            testHaskellDepends = [ base tasty ];
            description = "Strongly-typed file paths";
            license = lib.licenses.mit;
          };
    };
}
