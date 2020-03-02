{ system ? builtins.currentSystem
, withHoogle ? false
, kpkgs ? import ../kpkgs {}
}:

kpkgs.rp.project ({ pkgs, hackGet, ... }:
  let
    hs-abci = hackGet ./dep/hs-abci;
    pact = hackGet ./dep/pact;
    which = hackGet ./dep/which;
    tendermint = pkgs.callPackage ./dep/tendermint.nix {};

    overrides = with pkgs.haskell.lib; pkgs.lib.foldr pkgs.lib.composeExtensions  (_: _: {}) [
      (import hs-abci {}).overrides
      (self: super: {
        kadenamint = overrideCabal super.kadenamint (drv: {
          buildTools = (drv.buildTools or []) ++ [ pkgs.buildPackages.makeWrapper ];
          executableSystemDepends = (drv.executableSystemDepends or []) ++ [tendermint pkgs.z3];
          postFixup = ''
            ${drv.postFixup or ""}
            wrapProgram "$out"/bin/kadenamint --set SBV_Z3 ${pkgs.z3}/bin/z3
          '';
        });

        pact = dontCoverage (addBuildDepend super.pact pkgs.z3);

        tomland = dontCheck (self.callHackageDirect {
          pkg = "tomland";
          ver = "1.0.1.0";
          sha256 = "0m9x09f694rlkmvmrba79dcffzdy8abs6fs56j8a6525lx46vccl";
        } {});
      })
    ];

  in {
    inherit overrides withHoogle;

    passthru = {
      inherit overrides tendermint;
      inherit (pkgs) tmux;
      nixpkgs = pkgs;
      overrideDerivation = pkgs.lib.overrideDerivation;
    };

    packages = {
      inherit pact which;
      kadenamint = kpkgs.gitignoreSource ./.;
    };

    shells = {
      ghc = ["kadenamint"];
    };
  }
)
