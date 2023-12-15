# Comparsion

## Common

* Production readiness and performance
* Possibility of integration

## Аггрегация

* Вопросы
    * Где фильтровать
* Критерии
    * Нужный код: генерация фильтров, реакция и запросы
    * Сценарии: active utxo, реакция DApp и хранение в БД
    * Estimates на метрики производительности
    * Критерии: production-readiness, скорость, API simplicity, re-indexing
* Фильтрации вид
    * Никто не провайдит DSL (Carp/Oura умеют not fully documented Rust/Deno, visitorn)
    * Kupo - address, policy id or output reference
* Kupo
    * https://github.com/CardanoSolutions/kupo#alternatives

# Backends

## On-chain

* Plutus
* Plutarch

## Off-chain and testing

* Monads over cardano-api from Hydra Auction
* Crooked-validators, mutation solutions from Hydra project

## BMC backends

### Haskell fuzzing

* quickcheck-dynamic
* quickcheck-lockstep (?)

### Haskell BMC

* SBV

### Non-Haskell BMC

* Kind 2
    * Used for Djed.
* NuSMV
    * Was explored in pre-smart-contract era by
    "Model Checking Contracts – A Case Study" work.
    https://link.springer.com/chapter/10.1007/978-3-540-75596-8_8
    * For Ethereum by "Model-Checking of Smart Contracts"
    https://hal.science/hal-02103511/file/Nehai-Piriou-Daumas%20V18_03_09.pdf
* ESBMC
    * Used by ESBMC-Solidity
* TLA+
* Procella/SPIN
* Uppaal

