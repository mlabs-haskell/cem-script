# Solidity verification

## Comparsion of account-based and EUtXO verification

Solidity PL has commands for writing in Design-by-Contract style.
They fail dynamically, and could have been fuzzed for a while.
Same stands for arithmetic overflow and array bounds errors.
Recently they got symbolic execution support as well.

Overeflow and bound errors are less dangerous in Plutus,
because they cannot lead to silentl logic bug,
but they can be a reason for getting contract in stuck state as well.
Complications arising from external contract calls support,
and re-entrancy bugs in particular are absent in EUTxO model.

On other side, checks for unreachable code and balance/gas errors,
are just as important for Plutus as for Solidity.

Solidity contracts model resembles class-based programming,
and thus is not only very natural for using DbC style,
but overall much simpler to check with formal methods then EUTxO.

## Recent reviews

## Known solutions

List of known solutions based on this:

* SMTChecker Solidity compiler plugin
https://docs.soliditylang.org/en/latest/smtchecker.html
* HEVM/DApp tools - fuzzer and symbolic executor
    * Uses SMT for exploration
    * https://github.com/dapphub/dapptools/tree/master
* Echidna - fuzzer (writing in Haskell, pretty simple)
* Etheno

Other tools:

* Static analysis - TODO
* KEVM - operation semantics mechanization for EVM
