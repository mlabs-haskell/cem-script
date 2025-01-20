# CEM Script's getting started guide

## Basic Concepts

An instance of `CEMScript` revolves primarily around the following two type classes.

Here, `script` is an uninhabited type that is used to
tie together the different types associated with an instance of CEMScript.on

`CEMScriptTypes` defines those types:

```haskell
class CEMScriptTypes script where
  -- | `Params` is the immutable part of script Datum
  type Params script = params | params -> script

  -- | `State` is the changing part of script Datum.
  type State script = params | params -> script

  -- | Transitions for deterministic CEM-machine
  type Transition script = transition | transition -> script

  -- | Results of computations that run during each transition.
  -- | See `transitionComp` below.
  -- | This is optional.
  type TransitionComp script
  type TransitionComp script = NoTransitionComp
```

`CEMScript` defines the following functions:

```haskell
class
  ({- constraints elided for brevity -}) =>
  CEMScript script
  where
  -- | The crux part - a map that defines constraints for each transition via DSL.
  transitionSpec :: CEMScriptSpec False script

  -- | Optional Plutus script to calculate things, which can be used in the cases
  -- when CEM constraints and/or inlining Plutarch functions are not expressible
  -- enough.
  transitionComp ::
    Maybe
      ( Params script ->
        State script ->
        Transition script ->
        TransitionComp script
      )
  {-# INLINEABLE transitionComp #-}
  transitionComp = Nothing

  compilationConfig :: CompilationConfig

type CEMScriptSpec resolved script =
  ( Map.Map
      (Spine (Transition script))
      [TxConstraint resolved script]
  )
```

### Spine

The spine of an ADT is a sum type that is just a list of its constructors.

`HasSpine` type class maps from an ADT to its Spine type and:

- associates the type to its Spine type through a type family
- defines a function that translates an instance of the type to an instance of the spine type

```haskell
-- superclass constraints elided for clarity
class HasSpine sop where
  type Spine sop = spine | spine -> sop
  getSpine :: sop -> Spine sop
```

Using the Template Haskell, we provide a function `deriveSpine` to automatically derive `HasSpine`.

### DSL

When writing `CEMScript` transitions, we start by describing constraints
at a high level using a DSL that consists of the following types:

#### TxConstraint

```haskell
data TxConstraint (resolved :: Bool) script where
	-- elided --
```

`TxConstraint` is a GADT (Generalized Algebraic Data Type) parameterized by two things:

1. **`resolved`**: A Boolean type flag (usually `True` or `False`) indicating whether the constraint is in a "resolved" form.
In on-chain code, `resolved` will be `False`, representing the actual computation that’s going to take place.
In off-chain code, `resolved` will be `True`, which is used to derive a valid transaction from a list of `TxConstraint`s.
See the Off-chain machinery section for more details on how this works.
3. **`script`**: A phantom type parameter indicating which CEMScript (state machine) the constraints belong to.

See the reference section for a full reference of `TxConstraint`.

#### ConstraintDSL

`ConstraintDSL script value` is a GADT that represents a symbolic expression in the DSL.

- `script` is a phantom type parameter, just like in `TxConstraint`.
- `value` is the type of what this expression will resolve during runtime.

`ConstraintDSL` allows us to reference parts of the state machine's parameters, 
the current state, the transition arguments, and so forth.

It also lets us perform checks (like equality) and apply transformations (like lifting a Plutarch function).

See the reference section for a full reference of `ConstraintDSL`.

### DSLValue / DSLPattern

```haskell
type family DSLValue (resolved :: Bool) script value where
  DSLValue False script value = ConstraintDSL script value
  DSLValue True  _      value = value

type family DSLPattern (resolved :: Bool) script value where
  DSLPattern False script value = ConstraintDSL script value
  DSLPattern True  _      value = Void
```

These are both type-level wrappers over `ConstraintDSL`.

The type parameter `resolved` will be False for on-chain code, while it will be True for off-chain code.

In an unresolved `TxConstraint` (on-chain), they both assume the type `ConstraintDSL script value`

In a resolved `TxConstraint` (off-chain),

- `DSLValue _ value` assumes the type `value`
- `DSLPattern _ value` assumes the uninhabited type `Void`

    Any expression which contains a `DSLPattern`, like `If` and `MatchBySpine`, will get compiled away to the corresponding branch by evaluating the current state.

See the next **Off-Chain Machinery** section to understand how this works.

## Off-chain machinery

In addition to being a language to define on-chain code,
`CEMScript` offers the machinery to safely construct valid transactions
based on the on-chain state as well as the transition constraints.

```haskell
resolveTx ::
  forall m.
  (MonadQueryUtxo m, MonadSubmitTx m, MonadIO m) =>
  TxSpec ->
  m (Either TxResolutionError ResolvedTx)

data TxSpec = MkTxSpec
  { actions :: [SomeCEMAction]
  , specSigner :: SigningKey PaymentKey
  }
  deriving stock (Show)

data SomeCEMAction where
  MkSomeCEMAction ::
    forall script.
    (CEMScriptCompiled script) =>
    CEMAction script ->
    SomeCEMAction

data CEMAction script = MkCEMAction (Params script) (Transition script)

data ResolvedTx = MkResolvedTx
  { txIns :: [(TxIn, TxInWitness)]
  , txInRefs :: [TxIn]
  , txOuts :: [TxOut CtxTx Era]
  , toMint :: TxMintValue BuildTx Era
  , interval :: Interval POSIXTime
  , additionalSigners :: [PubKeyHash]
  , -- FIXME
    signer :: ~(SigningKey PaymentKey)
  }
  deriving stock (Show, Eq)

```

`resolveTx` is the primary entry point for off-chain code.
It accepts a  `TxSpec`, which consists of a list of actions and a signer,
and produces a `ResolvedTx`, which is all the information needed to construct
a transaction that’s ready to be submitted to the chain.

Here’s a rough outline of how it works:

- For each `Transition` in the list of actions
    - Find the corresponding transition in the `transitionSpec`
    - If the transition spec contains a `TxFan In SameScript`, check the current on-chain state is the same as required by the constraint.
    - Convert the `[TxConstraint (resolved :: False)]` of the `Transition` to `[TxConstraint (resolved :: True)]` by invoking `compileConstraint`.

        This evaluates all the `DSLValue _ value`  expressions and resolves it to a value of type `value`.

        The following variants of `TxConstraint`

        ```haskell
          = TxFan
              { kind :: TxFanKind
              , cFilter :: TxFanFilterNew resolved script
              , value :: ConstraintDSL script Value
              }
          | MainSignerCoinSelect
              { user :: ConstraintDSL script PubKeyHash
              , inValue :: ConstraintDSL script Value
              , outValue :: ConstraintDSL script Value
              }
          | MainSignerNoValue (ConstraintDSL script PubKeyHash)
        ```

        turns into

        ```haskell
          = TxFan
              { kind :: TxFanKind
              , cFilter :: TxFanFilterNew resolved script
              , value :: Value
              }
          | MainSignerCoinSelect
              { user :: PubKeyHash
              , inValue :: Value
              , outValue :: Value
              }
          | MainSignerNoValue PubKeyHash

        ```

        This also eliminates control structures that use `DSLPattern _ _`.

        The following variants

        ```haskell
          | If
              -- | Condition
              (ConstraintDSL script Bool)
              -- | Then block
              (TxConstraint resolved script)
              -- | Else block
              (TxConstraint resolved script)
          | forall sop.
            (HasPlutusSpine sop) =>
            MatchBySpine
              -- | Value being matched by its Spine
              (ConstraintDSL script sop)
              -- | Case switch
              (Map.Map (Spine sop) (TxConstraint resolved script))
        ```

        will become a single `TxConstraint` value based on which branch is taken based on the current state and the condition expression.

- Concatenate and deduplicate the thus obtained `[[TxConstraint (resolved :: True)]]`  to get a flat list `[TxConstraint (resolved :: True)]` .
- For each `TxConstraint`, query the current on-chain state and produce a value of type `Resolution`

    ```haskell
    data Resolution
      = TxInR (TxIn, TxInWitness)
      | TxInRefR (TxIn, TxInWitness)
      | TxOutR (TxOut CtxTx Era)
      | AdditionalSignerR PubKeyHash
      | NoopR
      deriving stock (Show, Eq)
    ```

- Use the list of `Resolution` thus obtained to build the `ResolvedTx` record.

The `ResolvedTx` can be used with an instance of `MonadSubmitTx` to submit it to the chain.

`MonadSubmitTx` is provided by default for CLB and local cardano node.

## Reference

### TxConstraint

```haskell
data TxConstraint (resolved :: Bool) script
  = TxFan
      { kind :: TxFanKind
      , cFilter :: TxFanFilterNew resolved script
      , value :: DSLValue resolved script Value
      }
  | MainSignerCoinSelect
      { user :: DSLValue resolved script PubKeyHash
      , inValue :: DSLValue resolved script Value
      , outValue :: DSLValue resolved script Value
      }
  | MainSignerNoValue (DSLValue resolved script PubKeyHash)
  | Error Text
  | If
      -- | Condition
      (DSLPattern resolved script Bool)
      -- | Then block
      (TxConstraint resolved script)
      -- | Else block
      (TxConstraint resolved script)
  | forall sop.
    (HasPlutusSpine sop) =>
    MatchBySpine
      -- | Value being matched by its Spine
      (DSLPattern resolved script sop)
      -- | Case switch
      -- FIXME: might use function instead, will bring `_` syntax,
      -- reusing matched var and probably implicitly type-checking spine
      -- by saving it to such var DSL value
      (Map.Map (Spine sop) (TxConstraint resolved script))
  | Noop
```

- `TxFan` Ensure that there exists a utxo with the specified constraints.
    - `kind`
        - `TxFanKind.In`: Utxo must be part of the inputs.
        - `TxFanKind.InRef`: Utxo must be part of the reference inputs.
        - `TxFanKind.Out` : Utxo must be part of the outputs.
    - `cFilter`
        - `UserAddress addr`: Utxo must have the given address
        - `SameScript state`: Utxo must be a script address belonging to the current script, and it’s datum must be equal to the given state value.
    - `value`: Utxo must have the given value. It is of the PlutusLedger `Value` type.
- `MainSignerCoinSelect user inValue outValue`

    Ensure

    - The sum of input utxos belonging to `user` is greater than `inValue`
    - The sum of output utxos belonging to `user` is greater than `outValue`
- `MainSignerNoValue address` Ensure that the given address is part of the transaction signatories.
- `Error message` Output `message` using `ptrace` as an error message.
- `If`
    - Evaluate the condition and resolve to the `TxConstraint` in the “then” branch or the “else” branch.
- `MatchBySpine pattern map` Evaluate the pattern and execute the corresponding action from `map`
    - `map` Map from `Spine` to `TxConstraint`

In addition to the above, CEMScript provides the following helper functions to create `TxConstraint`s

```haskell
cNot :: ConstraintDSL script Bool -> ConstraintDSL script Bool
cNot = LiftPlutarch pnot

offchainOnly :: TxConstraint False script -> TxConstraint False script
offchainOnly c = If IsOnChain Noop c

byFlagError ::
  ConstraintDSL script Bool -> Text -> TxConstraint False script
byFlagError flag message = If flag (Error message) Noop

```

- `cNot` inverts a boolean
- `offchainOnly` executes only on chain. Resolve to `Noop` off chain.
- `byFlagError` evaluates to `Error message` if `flag` evaluates to true.

### ConstraintDSL

```haskell
data ConstraintDSL script value where
  -- Request contextual values (parameters, state, transition, etc.)
  -- from the DSL environment during constraint evaluation
  Ask :: (..) => Proxy var -> ConstraintDSL script datatype
  -- Lifts pure values into the DSL context
  Pure :: ( .., ToData value') => value' -> ConstraintDSL script value'
  -- Are we running in the on-chain validator context or the off-chain code
  IsOnChain :: ConstraintDSL script Bool
  -- Accesses record fields in a type-safe way
  -- Evaluates to sop.label
  GetField :: (..) => ConstraintDSL script sop -> Proxy label -> ConstraintDSL script value
  -- Constructs a new value of a data type given its spine (constructor choice) and field values.
  -- DO NOT use this directly. Use `cOfSpine` instead.
  UnsafeOfSpine :: (..) => Spine datatype -> [RecordSetter (ConstraintDSL script) datatype] -> ConstraintDSL script datatype
  -- Similar to UnsafeOfSpine but updates an existing value, keeping unmodified fields.
  -- Used for efficient partial updates of data structures.
  UnsafeUpdateOfSpine :: (..) => ConstraintDSL script datatype -> Spine datatype -> [RecordSetter (ConstraintDSL script) datatype] -> ConstraintDSL script datatype

  -- Primitives

  -- A wildcard pattern that matches any value.
  -- Used in pattern-matching contexts where the actual value is irrelevant.
  Anything :: ConstraintDSL script x
  -- Compare two DSL values
  Eq :: (..) => ConstraintDSL script x -> ConstraintDSL script x -> ConstraintDSL script Bool
	-- Lifts single-argument Plutarch functions into the DSL context
  LiftPlutarch :: (..) => (ClosedTerm (px :--> py)) -> ConstraintDSL script (PLifted px) -> ConstraintDSL script (PLifted py)
	-- Lifts two-argument Plutarch functions into the DSL context
  LiftPlutarch2 :: (..) =>
    (forall s. Term s px1 -> Term s px2 -> Term s py) ->
    ConstraintDSL script (PLifted px1) ->
    ConstraintDSL script (PLifted px2) ->
    ConstraintDSL script (PLifted py)
```

Safe helpers

```haskell

-- Safe version of `UnsafeOfSpine`
cOfSpine :: (..) => Spine datatype -> [RecordSetter (ConstraintDSL script) datatype] -> ConstraintDSL script datatype
-- Safe version of `UnsafeUpdateOfSpine`
cUpdateOfSpine :: (..) => ConstraintDSL script datatype -> Spine datatype ->  [RecordSetter (ConstraintDSL script) datatype] -> ConstraintDSL script datatype
```

Helper functions for convenience

```haskell
cMkAdaOnlyValue ::
  ConstraintDSL script Integer -> ConstraintDSL script Value
cMkAdaOnlyValue = LiftPlutarch pMkAdaOnlyValue

cEmptyValue :: ConstraintDSL script Value
cEmptyValue = cMkAdaOnlyValue $ Pure 0

cMinLovelace :: ConstraintDSL script Value
cMinLovelace = cMkAdaOnlyValue $ Pure 3_000_000

(@==) :: (Eq x)
	=> ConstraintDSL script x
  -> ConstraintDSL script x
  -> ConstraintDSL script Bool
(@==) = Eq
```

The following are lifted versions of Plutarch operators

```haskell
(@<=) -- #<=
(@<) -- #<
(@<) -- #<
(@>=) -- #>=
(@>) -- #>

```

```haskell
-- Merge the contents of two `Value` instances.
(@<>) ::
  ConstraintDSL script Value ->
  ConstraintDSL script Value ->
  ConstraintDSL script Value
```

## Usage and Examples

### How to define a script

- Define an empty type for your script: `data MyScript`, where MyScript can be any name.
- Define a `CEMScript` instance for it by providing `Params`, `State`, `Transition`, `transitionSpec`, and optionally `transitionComp`.
- Do Template Haskell derivations (`deriveCEMAssociatedTypes`) to generate data and spine instances for pattern matching.
- Invoke the `compileCEM` function (e.g., `$(compileCEM True ''MyScript)`) to process the DSL specification, compile optional `transitionComp` code, and produce a `CEMScriptCompiled` instance.
    - This generates an instance of `CEMScriptCompiled` for your script type.
    - You may invoke `cemScriptCompiled` on your script type to get an instance of `Plutarch.Script`

### Example: Auction

#### Setup: The Types

First, we define a type to denote our script. It’s an uninhabited type, it can’t be constructed. 
It’s only used as a tag for connecting all instances of type classes together.

```haskell
data SimpleAuction
```

We define a type for the read-only state of our script. This state can’t be modified once created. 
This becomes the `Params` associated type of the `CEMScript` type class.

```haskell
data SimpleAuctionParams = MkAuctionParams
  { seller :: PubKeyHash,
    lot :: Value
  }
  deriving stock (Prelude.Eq, Prelude.Show)
```

We define a type for the evolving state of our script. This becomes the `State` associated type of the `CEMScript` type class.

```haskell
data Bid = MkBid
  { bidder :: PubKeyHash,
    bidAmount :: Integer
  }
  deriving stock (Prelude.Eq, Prelude.Show)

derivePlutusSpine ''Bid -- This will be explained below.

data SimpleAuctionState
  = NotStarted
  | CurrentBid
      { bid :: Bid
      }
  | Winner
      { bid :: Bid
      }
  deriving stock (Prelude.Eq, Prelude.Show)
```

Lastly, we define a type for the state transitions of our script. This becomes the `Transition` associated type of the `CEMScript` type class.

```haskell
data SimpleAuctionTransition
  = Create
  | Start
  | MakeBid
      { bid :: Bid
      }
  | Close
  | Buyout
  deriving stock (Prelude.Eq, Prelude.Show)
```

We can now define an instance of the `CEMScriptTypes` for `SimpleAuction`.
`CEMScriptTypes` is a superclass of `CEMScript`, which just includes the associated types. 
By defining the associated types separately, we can use the `deriveCEMAssociatedTypes` 
Template Haskell function to generate some boilerplate.

```haskell
instance CEMScriptTypes SimpleAuction where
  type Params SimpleAuction = SimpleAuctionParams
  type State SimpleAuction = SimpleAuctionState
  type Transition SimpleAuction = SimpleAuctionTransition

$(deriveCEMAssociatedTypes False ''SimpleAuction)
```

`deriveCEMAssociatedTypes` just executes `derivePlutusSpine` for all three of the associated types. 
But it can only do that if all the members of a type have a `HasPlutusSpine` implementation. 
This is why we need to do `derivePlutusSpine` for the `Bid` type ourselves.

The boolean argument to `deriveCEMAssociatedTypes` is unused for now, and it is recommended to use a value of `False`.

#### Implementation

To implement the logic of our script, we define an instance of `CEMScript` for our script type `SimpleAuction`

```haskell
instance CEMScript SimpleAuction where
```

We provide a value for `compilationConfig`, which at the moment contains 
only a prefix for error codes to tell errors from different programs apart.

```haskell
  compilationConfig = MkCompilationConfig "AUC"
```

Next comes the meat of the script: `transitionSpec`. This is where we define state transitions

We create a Map of `Spine (Transition script)` → `[TxConstraint False script]`

```haskell
  transitionSpec = Map.fromList
    [ .. ]
```

Before we go into the entries in detail, let’s define some helpers:

```haskell
buyoutBid = ctxState.bid

initialBid =
  cOfSpine
    MkBetSpine
    [ #bidder ::= ctxParams.seller
    , #bidAmount ::= lift 0
    ]

auctionValue = cMinLovelace @<> ctxParams.lot
```

We make extensive use of `OverloadedRecordDot` and custom `HasField` instances to make accessing the fields of the params and state values ergonomic.

Let’s examine each of the entries in the map in detail.

1. Create

    ```haskell
    ( CreateSpine
    ,
      [ spentBy ctxParams.seller cMinLovelace cEmptyValue
      , output (ownUtxo $ withNullaryState NotStartedSpine) auctionValue
      ]
    )
    ```

2. Start

    ```haskell
    ( StartSpine
    ,
      [ input (ownUtxo $ inState NotStartedSpine) auctionValue
      , output (ownUtxo $ withState CurrentBidSpine [#bid ::= initialBid]) auctionValue
      , signedBy ctxParams.seller
      ]
    )
    ```

3. MakeBid

    ```haskell
    ( MakeBidSpine
    ,
      [ input (ownUtxo $ inState CurrentBidSpine) auctionValue
      , byFlagError
          (ctxTransition.bid.bidAmount @<= ctxState.bid.bidAmount)
          "Bid amount is less or equal to current bid"
      , output
          ( ownUtxo
              $ withState
                CurrentBidSpine
                [#bid ::= ctxTransition.bid]
          )
          auctionValue
      , signedBy ctxTransition.bid.bidder
      ]
    )
    ```

4. Close

    ```haskell
    ( CloseSpine
    ,
      [ input (ownUtxo $ inState CurrentBidSpine) auctionValue
      , output
          ( ownUtxo
              $ withState WinnerSpine [#bid ::= ctxState.bid]
          )
          auctionValue
      , signedBy ctxParams.seller
      ]
    )
    ```

5. Buyout

    ```haskell
    ( BuyoutSpine
    ,
      [ input (ownUtxo $ inState WinnerSpine) auctionValue
      , byFlagError (lift False) "Some err"
      , byFlagError (lift False) "Another err"
      , -- Example: In constraints redundant for on-chain
        offchainOnly
          (if'
            (ctxParams.seller `eq'` buyoutBid.bidder)
            (signedBy ctxParams.seller)
            (spentBy
              buyoutBid.bidder
              (cMinLovelace @<> cMkAdaOnlyValue buyoutBid.bidAmount)
              cEmptyValue
            )
          )
      , output
          (userUtxo buyoutBid.bidder) -- NOTE: initial zero bidder is seller
          auctionValue
      , if'
          (ctxParams.seller `eq'` buyoutBid.bidder)
          noop
          ( output
            (userUtxo ctxParams.seller)
            (cMinLovelace @<> cMkAdaOnlyValue buyoutBid.bidAmount)
          )
      ]
    )
    ```
