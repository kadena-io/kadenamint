{ system ? builtins.currentSystem
, withHoogle ? false
}:

let
  rp = (import ./dep/pact {}).rp;

  recentNixpkgs = import (builtins.fetchTarball {
    name = "nixpkgs-pull-68945";
    url = https://github.com/nixos/nixpkgs/archive/7591343bf3bffdd139fce8a6b0f1c8e6c58ab6af.tar.gz;
    sha256 = "108qx0zja9cswq9kmh4vkwacppnw89rh6ibwf56kqfw8fqcwak0g";
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

    passthru = { nixpkgs = recentNixpkgs; };

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

        tomland = dontCheck (self.callHackageDirect {
          pkg = "tomland";
          ver = "1.0.1.0";
          sha256 = "0m9x09f694rlkmvmrba79dcffzdy8abs6fs56j8a6525lx46vccl";
        } {});
      })
    ];
  }
)
