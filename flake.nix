{
  description = "Strongly-typed file paths";

  inputs = {
    nixpkgs.url      = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    build-utils.url  = github:sixears/flake-build-utils/r1.0.0.11;

    base1t.url               = github:sixears/base1t/r0.0.5.20;
    exited.url               = github:sixears/exited/r1.0.4.16;
    has-callstack.url        = github:sixears/has-callstack/r1.0.1.12;
    monaderror-io.url        = github:sixears/monaderror-io/r1.2.5.13;
    more-unicode.url         = github:sixears/more-unicode/r0.0.17.8;
    non-empty-containers.url = github:sixears/non-empty-containers/r1.4.3.21;
    quasiquoting.url         = github:sixears/quasiquoting/r1.0.1.19;
    tasty-plus.url           = github:sixears/tasty-plus/r1.5.2.16;
    tfmt.url                 = github:sixears/tfmt/r0.2.7.15;
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

      callPackage = { mkDerivation, lib, system
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
            version = "1.3.2.21";
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
