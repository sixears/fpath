{
  description = "Strongly-typed file paths";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = "github:sixears/flake-build-utils/r1.0.0.6";

    base1t.url               = "github:sixears/base1t/r0.0.5.8";
    exited.url               = "github:sixears/exited/r1.0.4.8";
    has-callstack.url        = "github:sixears/has-callstack/r1.0.1.8";
    monaderror-io.url        = "github:sixears/monaderror-io/r1.2.5.7";
    more-unicode.url         = "github:sixears/more-unicode/r0.0.17.5";
    non-empty-containers.url = "github:sixears/non-empty-containers/r1.4.3.9";
    quasiquoting.url         = "github:sixears/quasiquoting/r1.0.1.9";
    tasty-plus.url           = "github:sixears/tasty-plus/r1.5.2.7";
    tfmt.url                 = "github:sixears/tfmt/r0.2.7.8";
  };

  outputs = { self, nixpkgs, flake-utils, build-utils
            , base1t, exited, has-callstack, monaderror-io, more-unicode
            , non-empty-containers, quasiquoting, tasty-plus, tfmt
            }:
    build-utils.lib.hOutputs self nixpkgs "fpath" {
      deps = {
        inherit base1t exited has-callstack monaderror-io more-unicode
                non-empty-containers quasiquoting tasty-plus tfmt;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
