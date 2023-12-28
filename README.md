[![PDD status](https://www.0pdd.com/svg?name=mlabs-haskell/cem-script)](https://www.0pdd.com/p?name=mlabs-haskell/cem-script)

# CEM SDK

* @todo #3 Take decision if to change project name to CEM SDK
* @todo #3 Found out if we can make PDD bot descriptions and mentions less noisy

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

## Project status

Project is in early development stage and is funded by
[Catalyst proposal](https://projectcatalyst.io/funds/10/f10-development-and-infrastructure/mlabs-cemscript-sdk-get-your-dapp-implementation-from-annotated-on-chain-logic-state-machine).
Detailed milestones of proposal and their status [are available](https://milestones.projectcatalyst.io/projects/1000118) as well.
