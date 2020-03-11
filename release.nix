{ system ? builtins.currentSystem }:
let self = import ./. {};
    cross = {
      inherit (self.ghc) kadenamint;
      shell = self.shells.ghc;
    };
in
  cross
