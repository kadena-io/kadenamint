{ system ? builtins.currentSystem
, withHoogle ? false
}:

let
  rp = (import ./dep/pact {}).rp;

  recentNixpkgs = import (builtins.fetchTarball {
    name = "nixos-master-2019-08-09";
    url = https://github.com/nixos/nixpkgs/archive/c2cc6aa6703be230d5590690735c0581de6c0216.tar.gz;
    sha256 = "1cxw3sg1bcaw40hsj634hfkq1szfr67gyxz44r680js9l6vvvwck";
  }) {};

  tendermint = recentNixpkgs.tendermint;

  purifyEnvironment =
    let isImpure = p: recentNixpkgs.lib.hasPrefix ".ghc.environment" p
                      || builtins.elem p [".git" "result" "dist-newstyle"];
    in builtins.filterSource (path: type: !isImpure (baseNameOf path));

in rp.project ({ pkgs, hackGet, ... }:
  let
    hs-abci = hackGet ./dep/hs-abci;
    pact = hackGet ./dep/pact;
    which = hackGet ./dep/which;

  in {
    inherit withHoogle;

    packages = {
      inherit pact which;
      kadenamint = purifyEnvironment ./.;
    };

    shells = {
      ghc = ["kadenamint"];
    };

    overrides = with pkgs.haskell.lib; pkgs.lib.foldr pkgs.lib.composeExtensions  (_: _: {}) [
      (import hs-abci {}).overrides
      (import (pact + /overrides.nix) pkgs)
      (self: super: {
        kadenamint = overrideCabal super.kadenamint (drv: {
          executableSystemDepends = (drv.executableSystemDepends or []) ++ [ tendermint ];
        });
      })
    ];
  }
)
