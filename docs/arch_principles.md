# Constraints design

## Principles

* Generic compilation across: on-chain code,
offchain Tx construction and indexing backend
* Simple normalization and SMT conversion for:
    * Equivalence checking
    * Bi-simulation checking
* Constraints determine TxIn/Outs up to UTxO coin-selection
(we call it almost-determinacy)
@todo #3: wording for almost-determinacy
* Datum properties encoded as class types
* Common on-chain optimizations are perfomed if possible
    * Constraints normalization,  and CSE
    * Best error short-cutting
    * Common security problems prevention

## Potential obstacles

* Ease and optimality of backend compilation
* Robustnes of SMT conversion and overall normalization
* Possibility for parsing and correct offchain usage
of almost-determinacy
* Having enough information for Tx submit retrying strategies
* Desing for using custom Datum properties is not obvious

# CEM machine design

As it is done on top of constraints language,
all their principles and obstacles are affecting CEM as well.

## Principles

* State-machine is deterministic modulo coin-change
    * Transaction can always be parsed back into SM transitions
    * Potential non-deterministic on-chain optimizations
    should not affect this principle.

## Potential obstacles

* Some scripts inexpressible by such model (as it happens in PAB)
* Sub-optimal code from determenistic transitions model
