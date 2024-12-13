module Cardano.CEM.Documentation (genCemGraph) where

import Cardano.CEM.Compile (transitionStateSpines)
import Cardano.CEM.DSL (
  CEMScript (perTransitionScriptSpec),
  CEMScriptTypes (Transition),
  TxFanKind (In, Out),
 )
import Data.Foldable (fold)
import Data.Map qualified as Map
import Data.Proxy (Proxy)
import Data.Spine (allSpines)
import Prelude

genCemGraph :: forall script. (CEMScript script) => String -> Proxy script -> String
genCemGraph name _proxy =
  "digraph "
    <> name
    <> " {\n"
    <> dotStyling
    <> edges
    <> "}"
  where
    edges =
      fold $
        [ from
          <> " -> "
          <> to
          <> " [label=\""
          <> showSpine transition
          <> "\"]; \n"
        | transition <- allSpines @(Transition script)
        , from <- get In transition
        , to <- get Out transition
        ]
    get kind transition =
      case transitionStateSpines kind $
        perTransitionScriptSpec @script Map.! transition of
        [] -> ["\"Void " <> show kind <> "\""]
        x -> map showSpine x

    showSpine :: (Show s) => s -> String
    showSpine = stripSpineSuffix . show

    stripSpineSuffix = reverse . drop 5 . reverse

    dotStyling :: String
    dotStyling =
      "rankdir=LR;\n"
        <> "node [shape=\"dot\",fontsize=14,fixedsize=true,width=1.5];\n"
        <> "edge [fontsize=11];\n"
        <> "\"Void In\" [color=\"orange\"];\n"
        <> "\"Void Out\" [color=\"orange\"];\n"
