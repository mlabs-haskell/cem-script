# CEM Script project: goals and design

## Why Cardano dApp modeling language is important?

While prototypes can be easily constructed with existing Cardano frameworks,
making it secure and production ready is a sisyphean task.

We believe, that onchain PL improvements by themselves could not change that.
Only higher-level modeling framework can lead to reliable dApp production.

In following enumerate our high-level design goals
and demonstrate them by list of specific problems
arising in existing dApps codebase.

Such problems are specific security vulnerabilities types
and various kinds development, audit and support cost increase.
After that we examine how existing solutions
are covering our high-level goals.

## High-level goals

1. dApp logic as a whole (synced-by-construction)
2. Code is free from common security weaknesses by construction (secure-by-construction)
3. Seamlessly emulate and test anything (emulate-anything)
4. Being declarative: close to informal specification and
bridging lightweight formal methods (declarative-spec)
5. Generally production ready (production-ready)

## The scope

We restrict the scope of the project by deliberately
keeping some features out of the scope now and in foreseable future.
Among them:

* Composable inter-dApp interactions
* dApp migrations
* Scheduled actions and bots

Some features that can be thought as a part of the project
but which are not in the Fund10 Catalyst proposal:

* CTL and overall frontend transaction construction model
* Metadata, reference scripts and non-inlined datums
* Stacking and governance features, apart for (security-by-default)
* ZK-proofs

## Reference dApps

Those are list of open-source dApps,
that we use to demonstrate problems is following:

* Audited production dApps
  * Agora
  * MinSwap
  * Fracada
  * JPG Vesting Contract
  * Indigo Protocol
* dApp projects we participated, audited or otherwise know their codebase
  * Hydra Auction
  * POCRE
  * CNS
* Public dApp projects with specification
  * [SundaeSwap](https://cdn.sundaeswap.finance/SundaeV3.pdf)
* Plutonomicon patterns
* Plutus tutorial
  * [Game Model](https://github.com/IntersectMBO/plutus-apps/blob/dbafa0ffdc1babcf8e9143ca5a7adde78d021a9a/doc/plutus/tutorials/GameModel.hs)
* plutus-usecases

## On-chain correctness

### Known common vulnerabilities

There is a list of known security vulnerabilities,
for which naive Plutus implementation is very often prone to.
Our implementation makes them impossible to happen,
taking that burden from developers and auditors.

* Double satisfaction
  * Shown twice in CNS Audit
  * Shown once in Fracada audit (3.2.7)
* Insufficient staking control
  * Shown once in CNS Audit - https://github.com/mlabs-haskell/cns-plutus-audit/issues/24

### TxIn ordering and coin change support

Those problems are similar to previous in that they tend to
arise in naive Plutus implementations,
if developers did not take measures to prevent them.

Plutus forces developers to write TxIn/TxOut constraints from scratch,
leading by subtle bugs from copy-pasting logic or
trying to optimize them by hand.

Examples:
* Security bug in MinSwap audit - 2.2.1.3 Unauthorized Hijacking of Pools Funds
* Non-security bug in MinSwap audit - 2.2.2.2 Batcher Is Not Allowed to Apply Their Own Order

Most of transactions which require fungible tokens as input,
should not depend on exact UTxO coin-change distribution.
Failure to expect that leads to prohibition of correct transactions.
On other side too broad constraint might lead to fund stealing.

See an example of such a [bug](https://github.com/mlabs-haskell/hydra-auction/issues/146).

Another important case is maintaining invariants
of token value or immutable UTxOs.
Such kind of constraints naturally lead to scripts
for which more performant implementation should
eliminate some constraint following from others.
Such kind of manual SMT solving exercises are
known source for security bugs and complicated code.
Checking all constraints leads to code bloat
in form of bunch of utility functions which is
suboptimal.
Making Plutus contract invariant to `TxIn` ordering
and participat* `availableVestings` - пример чего-то подобного для SimpleAuctionion in multi-script scenarios lead to
similar kind of complications.

Examples:
* Non-security bug: https://github.com/mlabs-haskell/hydra-auction/issues/129
* Non-security bug: https://github.com/mlabs-haskell/hydra-auction/commit/8152720c43732f8fb74181b7509de503b8371997
* Non-intentionally under-specified behavior in MinSwap audit:
  * `2.2.2.1 Batchers Can Choose Batching Order`
  * `2.2.4.1 "Reliance on Indexes Into ScriptContexts' txInputs and txOutputs"`
* Multiple kind of code complications was observed in CNS audit.
* Utilities [from Indigo](https://github.com/IndigoProtocol/indigo-smart-contracts/blob/main/src/Indigo/Utils/Spooky/Helpers.hs)

### Single script safety and liveliness

Main feature of any correct application is that it
behave same way as it specification presumes. In our case
specification for single script is a CEM state machine,
and correctness means that on-chain script have exactly
same transitions possible.

On-chain scripts having transition not possible in model
usually leads to security vulnerability. Other way around
it might be DoS vulnerability or just usability bug.
We aim to check most of such problems automatically.
Almost all on-chain code bugs are specific cases of
such violations, at least once we are analyzing
not single script, but a whole dApp, see the next section.

One very important example of liveliness violation
is originated from Cardano Tx limits. Such kind
of bugs are very hard to detect, without transaction fuzzing.
Such fuzzing can be automated as part of our testing framework.
Moreover, similar task of script usecase benchmarking
can be covered by same automation. Such kind of benchmarking,
either on list of scenarios or arbitrary generated inputs
is essential for checking economics and performance
regressions of dApps.

### Multiple script safety and liveliness

Whole dApp may be modeled as another kind of a state machine.
It operates a level above - the whole dApp and generates
specific script transitions.

Liveliness could be checked the same way with `quickcheck-dynamic`
or by providing specific usecases just as with single script case.

Safety checks is harder to automate.
This is not in the scope of current Catalyst project
but it should be possible to cover this case
by employing constraint equivalence check,
and verifying that any deviation from generated transitions
don't get through.
Another solution might be mutation testing.
Such kind of vulnerabilities are now the most complex kind of attack
to consider as part of dApps design or auditing.
Thus, automating it away may be very beneficial.

### Time and stages handling

Plutus time is described by allowed intervals.
Such abstraction is prone to off-by-one and similar errors.
At least three errors of this kind are known in Cardano/Plutus
interval logic, showing being not simple to implement
correctly.

Another problem is keeping time logic in sync between on-
and off-chain code. This is even harder given that Plutus time
to slot conversion gets into the way.
Slot time differences and overall need to make test cases match
real blockchain behavior may lead to flaky test behavior.

Our script stages abstraction cover all those kind of problems.

### Matching to off-chain logic

Problem of duplicating logic between on-chain and off-chain is twofold.
Testing is essentially done offchain, thus, one may easily miss
that your on-chain code is not actually enforcing some parts of
a transaction provided in tests.

CEM Script is constructing transactions for submission from exactly
the same specification used for on-chain script.
Thus it is much harder to miss constraint to be checked.

Examples:
* MinSwap audit - 2.2.1.2 LP Tokens Can Be Duplicated

### Logic duplication

There is a bunch of very common tasks shared by multiple dApps,
which could be tackled generically in framework like CEM Script.

## Human-readable specification

Designing, understanding and auditing any non-trivial dApp
is almost impossible without a human-readable spec.
That is why in all reference dApps there either existed
a spec or it was asked to be provided by an auditor.
Tweag and MLabs audits specifically list validating
provided specification as one of main tasks of an audit.

This leads to lot of cruft with writing, linking, and
updating specifications. Quite often not only one but
up to three levels of spec granularity which worsens
the situation.
We observe great amount of cruft and spec rot in all
projects we were personally involved.
Such kind of cruft is often present not only on level
of single developers, but on the community level.

Such kind of human-readable specs are very much similar to our model,
they almost always include enumeration of possible transitions
and/or state-machine diagrams.
Our project will generate such kind of specifications automatically.
Adding usecases scenarios generated from those specified in tests
is much less obvious to implement and out of scope of current project
but it is a very much possible feature as well.

Examples of this done by hand:
* [State graph for Agora](https://github.com/Liqwid-Labs/agora/blob/staging/docs/diagrams/ProposalStateMachine.png)

### On-chaion, off-chain, and spec logic duplication

Writing on-chain contracts manually encodes non-deterministic
state machine that cannot be used for off-chain transaction
construction.
Thus developer need to write them again in different style
for off-chain code which is tedious and error-prone.

They should add checks for all errors possible,
like coins being available and correct script states being present,
to prevent cryptic errors and provide retrying strategies
for possible utxo changes.

Our project encodes scripts in the form of a deterministic machine,
which contains enough information to construct transaction automatically.
This also gives a way to check for potential on-chain vs off-chain logic differences semi-automatically.

Examples:
* MinSwap Audit - 2.2.4.3 Large Refactoring Opportunities

### Computer readable spec and hashes

Script hashes and sizes summary is essential for dApp users
and testers to check on-chain script are matching.
We provide generic implementation showing all dApp hashes
via CIP.

### Indexing and querying code duplication

Our framework simplifies generation of common queries and
custom indexing.

Querying of current script state is required for almost all dApps,
and they are usually implemented with bunch of boilerplate.

Examples:
* [Querying available vestings](https://github.com/geniusyield/atlas-examples/blob/main/vesting/offchain/Vesting/Api.hs#L27)

Customized transaction indexing is important for providing
data to almost any kind of custom web-backend.
Customized indexing may reduce storage space or SaaS dependence.

Indexing transactions and parsing it back to state-machine transition
is required for delegated architectures, including many dApps and Hydra L2.

Examples:
* https://github.com/MELD-labs/cardano-defi-public/tree/eacaa527823031105eba1730f730e1b32f1470bc/lending-index/src/Lending/Index

## Existing solutions

This section covers some existing approches that can be
considered competative to CEM Script project.

## IOG's Plutus Application Framework

PABs `state-machines` and `plutus-contract-model` package
are the ony existing project close to our design.

They are providing CEM state machine model script,
translatable to on-chain and off-chain code.
On top of that you may specify custom `ContractModel`
specifying multi-script dApp as a whole,
and model check your custom invariants and
very generic "CrashTolerance" safety property
and "No Locked Funds" liveliness property.
Thus they are the only project known to us,
which aim to cover (declarative-spec)
and (synced-by-construction) goals.

This model is generic and covered in Plutus Pioneer tutorial,
thus already known to many people.
Our design is based on very similar model.
Unfortunately, existing `plutus-apps` solution seems
to be completely not (production-ready).
We could not find any `ContractModel` usage example on GitHub,
the repository is archived and thus it does seem that it is indeed
not much used in practice.
Also they provide much less free guaranties and features
then we aim to. As we will explain in the following sections,
this is most probably impossible to fix without significant
changes within design of PAB.

State Machine to on-chain translation is very naive
and would not produce optimal code in multiple typical cases.
Constraint language used by PAB is fundamentally more expressible
than ours, and that complicates possibility of implementing
required optimizations.

Same logic stands for (security-by-default) goal,
in particular providing SMT encoding for constrains,
which may be used to drastically shrink model-check fuzzing space
or even completely remove a need to use fuzzing.
Automatic check for various other important security properties,
like almost-determinism is complicated as well.

Another important restriction is the fact that PAB's
state machines always produce state-thread-token contract.
This makes impossible to encode tree like UTxO based data-structures,
which are used for encoding all kinds of public registry logic.
Also it prohibits any kind of non-trivial state-thread-token logic,
like optimization by sharing (used for example in `hydra-auction`)
and multiple commiters schemes (used in `hydra`).

### GeniusYield's Atlas PAB

Atlas provides more humane DX on top of `cardano-api`.
But it has no features related to the goals of
(synced-by-construction), (secure-by-construction),
and (declarative-spec).
Currently is covers (emulate-everything) though.

Atlas includes connectors to Blockfrost and other backends,
which our project lacks.

Also our project has slight differences in API design decisions.
Our monad interfaces is meant to be slightly more modular.
We use much less custom type wrappers, resorting to Plutus
types where possible.

### Tweag's cooked-validators

The project definitely covers the (production-ready) goal,
because it was successfully used in real-world audits.
but only partially covers
(emulate-anything) and (declarative-spec) goals.

While it, surely, can do transaction emulation,
it, to best of our knowledge, does not have monad instance
for real L1 blockchain. So one still cannot reuse same code
across tests and real blockchain Tx construction.

`TxSkel` datatype is similar to design we are implementing,
and other parts API have a lot of cool DX decisions.
But `cooked-validators` lack script transition model,
so it cannot provide declarative Tx submission error handling
we aim to get, because it does not have information to decide,
if specific Tx input is critical (like auth token),
or can be re-selected again (like coin-selected ADA for payment).

Having declarative transition model is even
more important for mutation testing purposes.
For construction of mutations `cooked-validators`
gives low-level Cardano API.
That means, that you should select, write and evaluate
them manually, depending on logic in scripts.
This lead to logic coupling between spec, L1 code and test code.
Such coupling and overall manual construction
complicates spec validation and might lead
to attack vectors missed by accident.

Our API presumes that means of on-script validation,
in our case behavior equivalence (AKA bi-similarity)
of a declarative CEM state machine and a real
on-chain script is verified and validated semi-automatically.
Also we plan to make some kind of vulnerabilities covered by
`cooked-validators` impossible by construction,
specifically in modules:
`ValidityRange`, `DoubleSat`, `AddInputsAndOutputs`
and `OutPermutations`.

Another important feature is support for LTL logic formulas,
which are known language for specifying state transitions in BMC.
It seems like this feature is subsumed by
`quickcheck-dynamic` based model checking specification
we are going to provide.

### Mutation-based testing of Hydra's validators

[Hydra mutations](https://abailly.github.io/posts/mutation-testing.html)
are similar to `cooked-validators` in their design
and have similar drawback.
Thus they do not cover (synced-by-construction),
(secure-by-construction),
and (declarative-spec) goals.

### tasty-plutus

Only cover (emulate-anything) goal, does not aim to cover others
and is not being developer either.
On the other hand it has some interesting low-level features,
like `plutus-size-check`.

### On-chain PLs and CIP-30 wrappers

Any kind of on-chain PL can only cover goals
(emulate-anything) and (production-readiness).
As far as we aware, none of them are trying
to solve other goals.

Known examples of PLs:

* [Marlowe](https://github.com/input-output-hk/marlowe-plutus)
  - Finance contracts DSL
* [Aiken](https://aiken-lang.org/)
  - on-chain PL with IDE support and testing framework
* [Helios](https://www.hyperion-bt.org/helios-book/api/index.html)
  - on-chain PL and frontend Tx sending library
* [OpShin](https://github.com/OpShin/opshin)
  - on-chain PL
* [Purus](https://github.com/mlabs-haskell/purus)
  - on-chain PL based on PureScript compiler (WIP by MLabs)

The same stands for any known kind of frontend frameworks:

* [CTL](https://github.com/Plutonomicon/cardano-transaction-lib)
* [lucid-evolution](https://github.com/Anastasia-Labs/lucid-evolution)
* [meshjs.dev](https://meshjs.dev/)

Same stands for any kind of `cardano-api` API translation to other PLs:

* [Transaction Village](https://github.com/mlabs-haskell/tx-village)
* [PyCardano](https://pycardano.readthedocs.io/en/latest/guides/plutus.html)
* [Cardano Multiplatform Lib](https://github.com/dcSpark/cardano-multiplatform-lib)

## Architectural principles

### Constraints design

* Generic compilation across:
    * on-chain code
    * off-chain tx construction
    * indexing backend
* Constraints determine input and outputs up to UTxO coin-selection (we call it almost-determinacy)
* Datum properties encoded as type classes
* Common on-chain optimizations are performed if possible
    * Constraints normalization
    * Best error short-cutting
    * Common security problems prevention

### CEM machine design

As it is done on top of constraints language,
all their principles and obstacles are affecting CEM as well:

* State-machine is deterministic modulo coin-selection
* Transaction can always be parsed back into CEM transitions
* Potential non-deterministic on-chain optimizations should not affect

### General Haskell verification tools

The definition of the property we aim to check is simple
and completely generic:

> Given a transaction with some UTxO context, and a function
> that generates SomeMutation from a valid transaction and
> context pair, this property checks applying any generated
> mutation makes the transaction invaild.

We are using `quickcheck-dynamic`library to run model-based
testing and mutation-based testing.