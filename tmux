DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# This is what one would expect
# "nix-shell -A shells.ghc -p tmux"
# to do, but it doesn't
cd $DIR && nix-shell -E 'with import ./. { }; passthru.overrideDerivation shells.ghc (drv: { buildInputs = [ passthru.tendermint passthru.tmux ]; })' --pure --run "./internal/clean && ./internal/tmux"
