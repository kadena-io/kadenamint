{ system ? builtins.currentSystem
, withHoogle ? false
}:

let
  rpRef = "f3ff81d519b226752c83eefd4df6718539c3efdc";
  rpSha = "1ijxfwl36b9b2j4p9j3bv8vf7qfi570m1c5fjyvyac0gy0vi5g8j";
  rpSrc = builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";
    sha256 = rpSha;
  };
  overlay = self: super: {
    z3 = super.z3.overrideAttrs (drv: {
      src = self.fetchFromGitHub {
        owner = "Z3Prover";
        repo = "z3";
        rev = "e79542cc689d52ec4cb34ce4ae3fbe56e7a0bf70";
        sha256 = "11sy98clv7ln0a5vqxzvh6wwqbswsjbik2084hav5kfws4xvklfa";
      };
    });
  };
  rp = import rpSrc {
    inherit system;
    nixpkgsOverlays = [ overlay ];
  };

  recentNixpkgs = import (builtins.fetchTarball {
    name = "nixos-master-2019-08-09";
    url = https://github.com/nixos/nixpkgs/archive/c2cc6aa6703be230d5590690735c0581de6c0216.tar.gz;
    sha256 = "1cxw3sg1bcaw40hsj634hfkq1szfr67gyxz44r680js9l6vvvwck";
  }) {};

  tendermint = recentNixpkgs.tendermint;

in rp.project ({ pkgs, hackGet, ... }:
  let
    hs-abci = hackGet ./dep/hs-abci;
    pact = hackGet ./dep/pact;

  in {
    inherit withHoogle;

    packages = {
      inherit pact;
      kadenamint = ./.;
    };

    shells = {
      ghc = ["kadenamint"];
    };

    overrides = with pkgs.haskell.lib; pkgs.lib.foldr pkgs.lib.composeExtensions  (_: _: {}) [
      (import hs-abci {}).overrides
      (self: super:
        let
          whenGhcjs = f: p: if self.ghc.isGhcjs or false then (f p) else p;
          callHackageDirect = {pkg, ver, sha256}@args:
            let pkgver = "${pkg}-${ver}";
            in self.callCabal2nix pkg (pkgs.fetchzip {
              url = "http://hackage.haskell.org/package/${pkgver}/${pkgver}.tar.gz";
              inherit sha256;
            }) {};

        in {
          # aeson 1.4.2
          aeson = (if self.ghc.isGhcjs or false
                   then (pkgs.lib.flip addBuildDepend self.hashable-time)
                   else pkgs.lib.id) (self.callCabal2nix "aeson" (pkgs.fetchFromGitHub {
                     owner = "bos";
                     repo = "aeson";
                     rev = "c3d04181eb64393d449a68084ffea3a94c3d8064";
                     sha256 = "1l8lks6plj0naj9ghasmkqglshxym3f29gyybvjvkrkm770p2gl4";
                   }) {});

          # need crackNum 2.3
          crackNum = dontCheck (self.callCabal2nix "crackNum" (pkgs.fetchFromGitHub {
            owner = "LeventErkok";
            repo = "crackNum";
            rev = "54cf70861a921062db762b3c50e933e73446c3b2";
            sha256 = "02cg64rq8xk7x53ziidljyv3gsshdpgbzy7h03r869gj02l7bxwa";
          }) {});

          swagger2 = whenGhcjs dontCheck (doJailbreak (callHackageDirect {
            pkg = "swagger2";
            ver = "2.3.1.1";
            sha256 = "0rhxqdiymh462ya9h76qnv73v8hparwz8ibqqr1d06vg4zm7s86p";
          }));
        })
      (import (pact + /overrides.nix) pkgs)
      (self: super: {
        kadenamint = overrideCabal super.kadenamint (drv: {
          executableSystemDepends = (drv.executableSystemDepends or []) ++ [ tendermint ];
        });
      })
    ];
  }
)
