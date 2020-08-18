# Using Kadenamint with Chainweaver


## Install and build with Nix

1. Go to https://nixos.org/ and click the `Download` button, then follow the Quick install instructions under the `Getting Nix` section.
2. Add `https://nixcache.chainweb.com` as a binary cache in `/etc/nix/nix.conf` with the public key `nixcache.chainweb.com:FVN503ABX9F8x8K0ptnc99XEz5SaA4Sks6kNcZn2pBY=`.  (see https://github.com/kadena-io/pact/wiki/Building-Kadena-Projects#building-with-nix-insert for a more detailed explanation.
3. Clone the kadenamint repository with `git clone https://github.com/kadena-io/kadenamint.git`.
4. `cd kadenamint`
5. `./run`

That will take some time to download kadenamint from the binary cache.  While that is running, install Chainweaver.

## Install Chainweaver 

Go to https://www.kadena.io/chainweaver and download and install the appropriate version.

Once you have Chainweaver installed and a wallet created, wait for the above
`run` command to finish. When it is ready you should see something like the
following lines:
   
```
I[2020-08-18|13:51:28.656] Executed block                               module=state height=23 validTxs=0 invalidTxs=0
I[2020-08-18|13:51:28.656] Committed state                              module=state height=23 txs=0 appHash=
```

with some green lines further up saying something like

```
[ABCI] Node: nodeX | Pact result:
  [ "Write succeeded"
  , {"sender00": 99999998.0,"sender01": 110000001.0,"sender02": 120000001.0} ]
```

## Connect Chainweaver to Kadenamint

1. Now go to the `Settings` menu (the cog icon in the bottom left).
2. Click `Network`.
3. Type `kadenamint1` in the `Create new network` field and click `Create`, then in the `Add node` field type `localhost:26659`.
4. Repeat the previous step to create a network called `kadenamint2` with `localhost:26669`.

This gives you a way to interact with two different nodes in the kadenamint
network by selecting `kadenamint1` or `kadenament2` in the Chainweaver networks
tab at the upper left corner of the screen.

The kadenamint demo starts with a coin contract called `coin` loaded and coins
in three accounts called `sender00`, `sender01`, and `sender02`. To see these
account balances, go to the `Accounts` screen, click `Add Account`, and then
type `sender00`. These are test accounts and the private keys for them are
available here:
https://github.com/kadena-io/chainweb-node/blob/master/pact/genesis/devnet/keys.yaml

You can use Chainweaver to transfer coins from these accounts to other accounts
by pasting the secret key into Chainweaver at the appropriate point in the
transfer process.

For more information about how to use Chainweaver to do things on the Kadenamint
network, see the Chainweaver users guide:

https://kadena-io.github.io/kadena-docs/Chainweaver-Support/
