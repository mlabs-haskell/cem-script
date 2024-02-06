# Design principles

## Why Cardano DApp modeling language is important?

While protototypes can be easily constructed with existing Cardano frameworks, making it secure and production ready is sisyphean task.

We believe, that onchain PL improvements by themself could not change that. Only higher-level modeling framework can lead to reliable DApp production.

In following enumerate our high-level design goals
and demonstrate them by list of specific problems
arising in existing DApps codebase.
Such problems are specific security vulnerabilies types
and various kinds development, audit and support cost increase.
After that we examine how existing solutions
are covering our high-level goals.

## High-level goals

1. DApp logic as whole (synced-by-construction)
2. Code is free from common security weaknesses by construction (secure-by-construction)
3. Seamlessly emulate and test anything (emulate-anything)
4. Declarativity close to informal specification and bridging lightweight formal methods (declarative-spec)
5. Generally production ready (production-ready)

## Not in scope at all

* Composable inter-DApp interactions and DApp migrations
* Scheduled actions

## Not in scope for current Catalyst funding

* CTL and overall frontend transaction construction model
* No specific support, but can be added manually
    * Metadata, reference scripts and non-inlined datums
    * Stacking and governance features (apart for (security-by-default))
    * ZK-proofs

# Details of examples of problems to be solved

## Reference apps

Those are list of open-source DApps,
what we use to demonstrate problems in folloving:

* Audited production DApps
    * Agora
    * MinSwap
    * Fracada
    * JPG Vesting Contract
    * Indigo Protocol
* DApp project we participate, audited or otherwise know it codebase
    * Hydra Auction
    * POCRE
    * CNS
    * Hydra
* Plutomicon patterns
* plutus-usecases

@todo #3: much more links to specific bugs and code size blowups

## On-chain correctness

### Known common vulnerabilities

There is a list of known security vulnerabilities,
for which naive Plutus implementation is very often prone to.
Our implementation makes them impossible to happen,
taking that burden from developers and auditors.

* Double satisfaction
    * Shown twice in CNS Audit
    * Shown once in Frada audit (3.2.7)
* Insufficient staking control
    * Shown once in CNS Audit - https://github.com/mlabs-haskell/cns-plutus-audit/issues/24

### TxIn ordering and coin change support

Those promlems are similar to previous in that they tend to
arise in naive Plutus implementations,
if developer was did not make measures to prevent them.

Almost all transactoins which require fungible tokens as input,
should not depend from exact UTxO coin-change distirbution.

Failure to expect that leads to prohibition of correct transactions.
On other side too broad constraint might lead to
fund stealing.

Example of bugs:

* https://github.com/mlabs-haskell/hydra-auction/issues/146

Another important case is maintaining invariants
of token value or immutable UTxOs.
Such kind of constraints naturally lead to script
for which more performant implementation should
eliminate some constraint following from others.
Such kind of manual SMT solving excercises are
known source for security bugs and compilcated code.

Making Plutus contract invariant to `TxIn` ordering
and participation in multi-script scenarios lead to
similar kind of complications.

Examples:

* Non-security bug: https://github.com/mlabs-haskell/hydra-auction/issues/129
* Non-security bug: https://github.com/mlabs-haskell/hydra-auction/commit/8152720c43732f8fb74181b7509de503b8371997
* Multiple kind of code complication was observed in CNS audit.

### Single script safety and liveliness

Main feature of any correct application is that it
behave same as it specification presumes. In our case
specification for single script is CEM state machine,
and correctnes means that on-chain script have exactly
same transitions possible.

On-chain script having transition not possible in model
usually leads to security vulnerablity. Other way around
it might be DoS vulnerability or just usability bug.
We aim to check most of such problems automatically.
Almost all on-chain code bugs are specific cases of
such violations, at least once we are analysing
not single script, but whole DApp, which is covered
in next section.

One very important example of liveliness violation
is originated from Cardano Tx limits. Such kind
of bugs are very hard to detect, without transaction fuzzing.
Such fuzzing can be automated as part of our testing framework.
Moreover similar task of script usecase benchmarking
can be covered by same automation. Such kind of benchmarking,
either on list of scenerios or arbitraly generated inputs
is essential for checking economics and performance regressions of DApp.

https://plutus.readthedocs.io/en/latest/reference/writing-scripts/common-weaknesses/index.html

### Multiple script safety and liveliness

Whole DApp may be modeled as another kind of state-machine.
It operates on level of whole DApp and generates specific script transitions. Liveliness could be checked by `quickcheck-dynamic`,
or providing specific usecases just as single script case.

Safety checks is harder to automate. This is not in the scope of current Catalyst project it is possbile to cover this case as well
by employing constraint equivalence check,
and verifying that any deviation from generated transitions
is not possible. Another solution might be mutation testing.
Such kind of vulnerabilities are now the most complex kind of attack
to consider as part of DApp design or auditing.
Thus, automaing it may be benefitial.

### Time and stages handling

Plutus time is described by allowed intervals.
Such abstraction is prone to off-by-one and similar kind of errors.
At least three errors of this kind are known in Cardano/Plutus
interval logic, showing that this is not simple to implement
correctly.

Another problem is keeping time logic in sync between on-
and off-chain code. This is even more hard given that Plutus time
to slot conversion is not obvious to implement correctly.
Slot time differences and overall need to make test cases match
real blockchain behaviour may lead to flacky test behaviour.

Our script stages abstraction cover all those kind of problems.

* @todo #3: document problems with slots
    * https://github.com/mlabs-haskell/hydra-auction/issues/236
* @todo #3: bug example


## Logic duplication and spec subtleness

### Human-readable specification

Designing, understanding and auditing any non-trivial DApp
is almost impossible without human-readable spec.
That is why in all reference DApps either used spec in development,
or was asked to provide specification by auditor.
Tweag and MLabs audits specificalli list validating provided specification
as one of main tasks of audit.

This leads to lot of cruft with writing, linking and updating specifications. Quite often not only one, but up to three levels of spec granularity would be benefitial for project, which worstens situation.
We observe great amount of cruft and spec rot in all projects we were personally involved. Such kind of cruft is often present
not only on level of single developers, but on communication

Such kind of human-readable specs are very much similar to our model,
they almost always include enumeration of possible transitions
and/or state-machine diagrams.
Our project will generate such kind of specifications automatically
as Markdown file with Mermaid diagrams.
Adding usecases scenarios generated from one specified in tests
is much less obvious to implement,
and out of scope of current Catalyst project,
but it is very much possible feature as well.

### On-/Off-chain and spec code duplication

@todo #3: more integration point code duplication examples

### Correct off-chain Tx construction logic

A lot of on-chain problems, like timing and coin change issues
have their counterpart on Tx sumbmission side.

@todo #3: coin-selection, tests, retrying and errors

### Comon backend features

There is a list of common tasks shared by multiple backends,
which could be tackled generically in our framework.

* Parsing transaction back to state-machine transition
is required for delegated architecures,
including almost any DApp on Hydra L2.
* Customized transation indexing is imporant for providing
data to almost any kind of custom web-backend.
Also usage of customized indexing may reduce storage space or SaaS dependence for DApp dependent on old transactions being recorded.
Our framework simplifies generation of custom indexing solution,
based on transition parsing feature.
* Script hashes and sizes summary is essential
for DApp users and testers to check on-chain script are matching.

# Existing solutions

## Backends

### PAB

PABs state-machines and `plutus-contract-model` package
are the ony existing project close to our design.

They are providing CEM state machine model scirpt,
translatable to on-chain and off-chaing code.
On top of that you may specify custom `ContractModel`
specifying multy-script DApp as a whole,
and model check your custom invariants and
very generic "CrashTolerance" safety property
and "No Locked Funds" liveliness propery.
Thus they are the only project known to us,
which aim to cover (declarative-spec) and (synced-by-construction) goals.

This model is generic and covered in Plutus Pioneer tutorial,
thus already known to many people.
Our design is based on very similar model.
Unfortunately, existing `plutus-apps` solution seem to be compeletely
not (production-ready). We could not find any `ContractModel` usage example on github, apart from forks and tutorials, thus it does not seem that it is indeed not much used in practice.
Also they provide much less free guaranties and features then we aim to.
As we will demonstrate in sequel,
this is most probably imposible to fix without changing PAB design.

State Machine to on-chain translation is very naive
and would not produce optimal code in multiple typical cases.
Constaint language used by PAB is fundmentaly more expressible than ours,
and that complicates possiblity of implementing required optimizations.
Specifics of those desing considerations are detailed in arch documention.

Same logic stands for (security-by-default) goal,
in particular providing SMT encoding for constrains,
which may be used to drastically shrink model-check fuzzing space
or even completely remove a need to use fuzzing.
Automatical check for varous other important security properties,
like almost-determinism is complicated by this as well.

Another important restriction,
is that PAB state machines always produce state-thread-token contract.
This makes impossible to encode tree like UTxO based data-structures,
which are used for encoding all kinds of public registry logic.
Also it prohibits any kind of non-trivial state-thread-token logic,
like optimization by sharing (used for example in `hydra-auction`)
and multiple commiters schemes (used in `hydra`).

@todo #3: Write more about PAB issues with emulator, packaging and Plutus

### Atlas

Atlas provides (emulate-everything) and overall more humane DX
on top of cardano-api. But it has no feature related to goals
(synced-by-construction), (secure-by-construction)
and (declarative-spec).

@todo #3: expand docs on Atlas

## Testing tools

### Tweag's cooked-validators

Project definetly covers goal (production-ready),
because it was sucessfully used in real-world audits.
@todo #3: provide examples
but only partially covers
(emulate-anything) and (declarative-spec) goals.

While it, surely, can do transaction emulation,
it, to best of our knoweledge, does not have monad instance
for real L1 blockchain. So you still cannot reuse same code
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
`cooked-validators` gives Cardan Tx level API,
for constructoion of mutations.
That means, that you should select, write and evaluate
them manually, depending on logic of your script.
This lead to logic coupling between spec, L1 code and test code.
Such coupling and overall manual construction
complicates spec validation and might lead
to attack vectors missed by mistake.

Our API presumes that means of on-script validation,
in our case behavour equivalence (aka bi-similarity)
of delaratime CEM state-machine and real on-chain script
is verified and validated semi-automatically.
Also we plan to make some kind of vulerabilities covered by
`cooked-validators` imposible by constuction, specifically modules:
`ValidityRange`, `DoubleSat`, `AddInputsAndOutputs`
and `OutPermutations`.

Another important feature is support for LTL logic formulas,
which are known language for specifying state transitions in BMC.
It seems like this feature is subsumed by
`quickcheck-dynamic` based model checking specification
we are going to provide.

### Hydra Mutations

Hydra mutations are similar to cooked-validators in their design
and have similar drawback. Thus they do not cover (synced-by-construction), (secure-by-construction)
and (declarative-spec) goals.

https://abailly.github.io/posts/mutation-testing.html

@todo: examples of test duplication and existing bugs

### tasty-plutus

Only cover (emulate-anything) goal, does not aim to cover others.
Is deprecated as well.
But it has some interesting low-level features,
like `plutus-size-check`, which code we would probably steal.

### General Haskell verification tools

We plan to use first two tools in our implementation.
They both are tested in real prjects and documented well.

Other tools are not applicable to our project.

1. quickehck-dynamic
2. SMV
3. apropos
4. Liquid Haskell
5. Agda

## On-chain PLs and CIP-30 wrapers

Any kind of on-chain PL can only cover goals
(emulate-anything) and (production-readyness).
As far as we aware, none of them are trying
to solve other goals.

Known examples:

* Aiken
* Helios
* https://github.com/OpShin/opshin
* PureScript Backend project

Same stands for any known kind of frontend framework:

* CTL
* Lucid
* meshjs.dev

Same stands for any kind of `cardano-api` API translation to other PLs:

* PyCardano - https://pycardano.readthedocs.io/en/latest/guides/plutus.html
* Cardano Multiplatform Lib

## Manual formalizations

* CEM-machines
* Manual Djed formalization with Kind 2/Lustre model
