# CEM Script

## Project pitch

CEM Script is a framework to define Cardano DApp logic via annotated CEM-machines,
resulting in free implementations for:

* On-chain scripts
* Transaction building/submission (off-chain)
* Transaction parsing/indexing
* Automatically testing invariants
* Human-readable specs

## Documentation

* [Getting Started Guide](https://github.com/mlabs-haskell/cem-script/blob/master/docs/getting_started.md)
* [Goals and Design](https://github.com/mlabs-haskell/cem-script/blob/master/docs/goals_and_design.md)
* Article about [testing dApps on Cardano with CLB](https://www.mlabs.city/blog/testing-dapps-on-cardano-with-clb-emulator) is another introduction to testing CEM Script dApps.

## Building

Building is performed with `cabal`inside IOG's `github:input-output-hk/devx` shell.
See `.envrc` for details.

Make sure to `cabal update` before building.

## Running tests

Tests are runned in emulated environment using
[CLB](https://github.com/mlabs-haskell/clb).

Just run: `cabal run cem-script-test`.