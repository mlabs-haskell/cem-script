# CEM SDK

## Project pitch

Define and reuse Cardano DApp logic via annotated CEM-machines, resulting in free implementations for:

* On-chain scripts
* Tx building/submission/resubmission on L1/emulated testnet
* Tx parsing/indexing
* Automatically testing invariants
* Human-readable specs

## Building

Building is performed with cabal.
Building requires `libblst` and `libsodium` installed.

Arch Linux has `libblst` in AUR, nix are exemplified by IOHK,
and manual installation is described here:
https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/getting-started/install.md#installing-blst

## Running tests

Tests are runned in emulated environment by default.

Just run: `cabal test`.

For development and fast response once could consider `ghcid`.

## Starting local devnet

Tests depend on localdevnet, which is runned in Docker.
To start it do:

```bash
./prepare-devnet.sh
docker-compose  -f docker-compose.devnet.yaml up
sudo chown -R $USER:$USER ./devnet/
```

## Devnet stalling bug

Sometimes devnet stalls, due to some bug, in that case one should restart it,
and wipe directory `./devnet/db`. To look for stalling one could check:
`CARDANO_NODE_SOCKET_PATH=./devnet/node.socket cardano-cli query tip --testnet-magic 42`. For properly working devnet slots should change
and sync be marked as 100%.

On this bug:
https://forum.cardano.org/t/restarting-custom-private-networks-cardano-node-forge35/116921

## Project status

Project is in early development stage and is funded by
[Catalyst proposal](https://projectcatalyst.io/funds/10/f10-development-and-infrastructure/mlabs-cemscript-sdk-get-your-dapp-implementation-from-annotated-on-chain-logic-state-machine).
Detailed milestones of proposal and their status [are available](https://milestones.projectcatalyst.io/projects/1000118) as well.
