# Milestone 5

## Summary of deliverables

* L1 indexing:
  * [Basic indexing support PR #98](https://github.com/mlabs-haskell/cem-script/pull/98)
  * [Oura config generation PR #100](https://github.com/mlabs-haskell/cem-script/pull/100)
  * [Source](https://github.com/mlabs-haskell/cem-script/blob/master/src/Cardano/CEM/Indexing.hs)
  * [Indexing tests for Auction example](https://github.com/mlabs-haskell/cem-script/blob/master/test/OuraFilter/Auction.hs)
* Final code clean-up:
  * [PR#113 - finalize mutation tests](https://github.com/mlabs-haskell/cem-script/pull/113)
  * [PR#109 - cosmetic changes](https://github.com/mlabs-haskell/cem-script/pull/109)
* Final tutorial and docs:
  * [PR on docs](https://github.com/mlabs-haskell/cem-script/pull/115)
    * [Getting Started Guide](https://github.com/mlabs-haskell/cem-script/pull/115/files#diff-31bcba2ccafa41d46fbbd6d1219f7f1e3b1fb3cad9faa8e4dc521bbb579dd7b3)
    * Updated [Goals And Design](https://github.com/mlabs-haskell/cem-script/pull/115/files#diff-ef1a1e144302d41f2687f34dc1885cd8434e6395aa5443c81a2bca8414911972)
* [Closeout video](TODO:link)

## Clarifications on M5 deliverables

in the final milestone we added support for indexing CEM Scripts based on
[Oura](https://github.com/txpipe/oura) utility. The framework provide utilities
to generate Oura's configs and parse transaction from Oura-based format into
CEM Script transitions.

The documentation has been reworked. Getting started guide has beeen added.
An introductory video on CEM Script has been recorded and published on YouTube.

# Milestone 4

## Summary

Catalyst Milestone Outputs:

* Static DSL for constraints and Plutarch on-chain code generation and optimisation ([PR#96](https://github.com/mlabs-haskell/cem-script/pull/96))
* Profiling implementation ([PR](https://github.com/mlabs-haskell/cem-script/pull/95))
* Profiling performed for old (using Plutus) and new (using Plutarch) code generator.
  New one showed advantage over first one in all fees measured, and in some cases in order of magntude.
* A [video](https://www.youtube.com/watch?v=wveLyvuKjeI) that summarizes the outcomes achieved in the milestone.

## Profiling results

### Old (Plutus) backend ([implementation commit](https://github.com/mlabs-haskell/cem-script/tree/7e39ee5cbb8b512f873de05af3573bda1355d0aa))

Auction flow:

```
  [ ( "BuyoutSpine"
    , MkFees
        { fee = Coin 1072912
        , usedMemory = 7642581
        , usedCpu = 2334110505
        }
    )
  , ( "CloseSpine"
    , MkFees
        { fee = Coin 1240433
        , usedMemory = 9509189
        , usedCpu = 3008756651
        }
    )
  , ( "CreateSpine"
    , MkFees { fee = Coin 198369 , usedMemory = 0 , usedCpu = 0 }
    )
  , ( "MakeBidSpine"
    , MkFees
        { fee = Coin 1240548
        , usedMemory = 9489565
        , usedCpu = 3001647940
        }
    )
  , ( "StartSpine"
    , MkFees
        { fee = Coin 1182093
        , usedMemory = 8782473
        , usedCpu = 2783614841
        }
    )
  ]
```

Voting flow (fails exceeding Tx limits on second vote):

```
  [ ( "CreateSpine"
    , MkFees { fee = Coin 190141 , usedMemory = 0 , usedCpu = 0 }
    )
  , ( "StartSpine"
    , MkFees
        { fee = Coin 724655 , usedMemory = 2961157 , usedCpu = 943387589 }
    )
  , ( "VoteSpine"
    , MkFees
        { fee = Coin 1164933
        , usedMemory = 8369793
        , usedCpu = 2680589904
        }
    )
  ]
```

### New (Plutarch with user-defined Plutus logic pieces) backend

Auction:

```
  [ ( "BuyoutSpine"
    , MkFees
        { fee = Coin 586779 , usedMemory = 625199 , usedCpu = 234106047 }
    )
  , ( "CloseSpine"
    , MkFees
        { fee = Coin 667350 , usedMemory = 1358848 , usedCpu = 611301346 }
    )
  , ( "CreateSpine"
    , MkFees { fee = Coin 198237 , usedMemory = 0 , usedCpu = 0 }
    )
  , ( "MakeBidSpine"
    , MkFees
        { fee = Coin 669571 , usedMemory = 1364475 , usedCpu = 613191159 }
    )
  , ( "StartSpine"
    , MkFees
        { fee = Coin 747648 , usedMemory = 2231662 , usedCpu = 1028952087 }
    )
  ]
```

Voting Plutarch backend:

```
  [ ( "CreateSpine"
    , MkFees { fee = Coin 190009 , usedMemory = 0 , usedCpu = 0 }
    )
  , ( "StartSpine"
    , MkFees
        { fee = Coin 495528 , usedMemory = 181513 , usedCpu = 72358787 }
    )
  , ( "VoteSpine"
    , MkFees
        { fee = Coin 646247 , usedMemory = 1882546 , usedCpu = 760595491 }
    )
  ]
```

# Milestone 3

## Summary

Changes:

* Running in emulated environment by CLB
  ([source](https://github.com/mlabs-haskell/cem-script/blob/master/src/Cardano/CEM/Monads/CLB.hs), usage examples are all unit tests)
* Running Quickcheck Dynamic tests, including mutation support
  ([source](https://github.com/mlabs-haskell/cem-script/blob/master/src/Cardano/CEM/Testing/StateMachine.hs),
  [usage example](https://github.com/mlabs-haskell/cem-script/blob/master/test/Dynamic.hs),
  [PR](https://github.com/mlabs-haskell/cem-script/pull/75) and [PR2](https://github.com/mlabs-haskell/cem-script/pull/89))
* Rendering CEMScript state graphs
  ([source](https://github.com/mlabs-haskell/cem-script/blob/master/src/Cardano/CEM/Documentation.hs), rendered example below,
  [PR](https://github.com/mlabs-haskell/cem-script/pull/33))

## State graph examples

![](./auction-state-graph.svg)


Source code:

```
digraph Creator {
rankdir=LR;
node [shape="dot",fontsize=14,fixedsize=true,width=1.5];
edge [fontsize=11];"Void In" [color="orange"];"Void Out" [color="orange"];"Void In" -> NotStarted [label="Create (stage Open)"];
NotStarted -> CurrentBid [label="Start (stage Open)"];
CurrentBid -> CurrentBid [label="MakeBid (stage Open)"];
CurrentBid -> Winner [label="Close (stage Closed)"];
Winner -> "Void Out" [label="Buyout (stage Closed)"];
}
```
