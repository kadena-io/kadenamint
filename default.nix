{ system ? builtins.currentSystem
, withHoogle ? false
}:

let
  rpRef = "d68ab80e3a92425d4bcbfa2da7c685aded550c16";
  rpSha = "0d5hc4v56pldy65rl0pmdzx2jylh8rjqqg5n0qb48039yy3pp8cq";

  rpSrc = builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";
    sha256 = rpSha;
  };
  rp = import rpSrc { inherit system; };
in rp.project ({ pkgs, hackGet, ... }:
  let
    hs-abci = hackGet ./dep/hs-abci;

  in {
    inherit withHoogle;

    packages = {
      kadenamint = ./.;
    };

    shells = {
      ghc = ["kadenamint"];
    };

    overrides = pkgs.lib.foldr pkgs.lib.composeExtensions  (_: _: {}) [
      (import hs-abci {}).overrides
    ];
  }
)
