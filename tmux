DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# This is what one would expect
# "nix-shell -A shells.ghc -p tmux"
# to do, but it doesn't
cd $DIR && nix-shell -E 'with import ./. { }; passthru.nixpkgs.lib.overrideDerivation shells.ghc (drv: { buildInputs = [ passthru.nixpkgs.tendermint passthru.nixpkgs.tmux ]; })' --pure --run "./internal/tmux"
