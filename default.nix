{ system ? builtins.currentSystem
, withHoogle ? false
}:

let
  rp = (import ./dep/pact {}).rp;

in rp.project ({ pkgs, hackGet, ... }:
  let
    hs-abci = hackGet ./dep/hs-abci;
    pact = hackGet ./dep/pact;
    which = hackGet ./dep/which;
    tendermint = pkgs.callPackage ./dep/tendermint.nix {};

    purifyEnvironment =
      let isImpure = p: pkgs.lib.strings.hasPrefix ".ghc.environment" p
                        || builtins.elem p [".git" "result" "dist-newstyle"];
      in builtins.filterSource (path: type: !isImpure (baseNameOf path));

  in {
    inherit withHoogle;

    passthru = {
      inherit tendermint;
      inherit (pkgs) tmux;
      nixpkgs = pkgs;
      overrideDerivation = pkgs.lib.overrideDerivation;
    };

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
          buildTools = (drv.buildTools or []) ++ [ pkgs.buildPackages.makeWrapper ];
          executableSystemDepends = (drv.executableSystemDepends or []) ++ [ tendermint pkgs.z3];
          postFixup = ''
            ${drv.postFixup or ""}
            wrapProgram "$out"/bin/kadenamint --set SBV_Z3 ${pkgs.z3}/bin/z3
          '';
        });

        pact = dontCoverage super.pact;

        tomland = dontCheck (self.callHackageDirect {
          pkg = "tomland";
          ver = "1.0.1.0";
          sha256 = "0m9x09f694rlkmvmrba79dcffzdy8abs6fs56j8a6525lx46vccl";
        } {});
      })
    ];
  }
)
