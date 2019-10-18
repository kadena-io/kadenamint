# Kadenamint

![Introducing Kadenamint](https://miro.medium.com/max/1156/1*ioGNdm7WNixtlNPLKwo9eQ.png)

Kadenamint is an implementation of the [Pact](https://github.com/kadena-io/pact/) smart contract language on [Tendermint](https://github.com/tendermint/tendermint).

The announcement blogpost can be found [here](https://medium.com/kadena-io/introducing-kadenamint-and-chainweb-testnet-v2-cde077c02ebc).

For more questions, you can join Kadena's [discord](https://discordapp.com/invite/bsUcWmX).

# Contracts
On the Genesis block, Kadenamint loads Pact's [coin contract](./pact/coin-contract/coin.pact), sets up some [initial accounts](pact/coin-contract/grants.yaml) with funds, and their [keys](pact/coin-contract/keys.yaml) for demonstration purposes. You can then broadcast transactions with Pact code to manipulate these accounts (e.g. transfer funds).

# Hacking

## Dependencies
The Nix package manager is required for developing on this project. You can download it by going [here](https://nixos.org/nix/) and clicking "Get Nix".
All other dependencies are handled by Nix. Note that Nix does not currently work on Windows - you will need either a virtual machine or [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10).

To speed up the initial builds you can add public binary caches by following these [instructions](https://github.com/kadena-io/pact/wiki/Building-Kadena-Projects#building-with-nix-insert).


## Developing
Several workflows are covered by the scripts at the root of the repository - they might take a long while to run the first time since Nix might not have cached the dependencies yet:

##### `hoogle`
Launches a [hoogle](https://hoogle.haskell.org/) server locally, with documentation for the exact versions of the Haskell dependencies in use.

##### `repl`
Launches an Haskell repl where networks/nodes can be controlled interactively.

##### `test`
Like `watch`, but will also run a Kadenamint network in a temporary directory and broadcast some coin transfer transactions, after compilation.

##### `tmux`
Launches a [tmux](https://github.com/tmux/tmux/wiki) session with a Tendermint testnet initialized. Adds an extra panel for each node with an Haskell repl where the prompt is prepopulated by the command to launch the node.

##### `watch`
Continously monitors the source code for errors and warnings, reloading on changes.