module Cardano.CEM.Documentation (cemDotGraphString) where

import Prelude

import Data.Foldable (fold)
import Data.Map qualified as Map
import Data.Proxy

import Cardano.CEM
import Cardano.CEM.DSL (transitionStateSpines)
import Data.Spine (allSpines)

dotStyling :: String
dotStyling =
  "rankdir=LR;\n"
    <> "node [shape=\"dot\",fontsize=14,fixedsize=true,width=1.5];\n"
    <> "edge [fontsize=11];\n"
    <> "\"Void In\" [color=\"orange\"];\n"
    <> "\"Void Out\" [color=\"orange\"];\n"

-- FIXME: cover with golden test
cemDotGraphString ::
  forall script. (CEMScript script) => String -> Proxy script -> String
cemDotGraphString name _proxy =
  "digraph "
    <> name
    <> " {\n"
    <> dotStyling
    <> edges
    <> "}"
  where
    showSpine :: (Show s) => s -> String
    showSpine = stripSpineSuffix . show
    stripSpineSuffix = reverse . drop 5 . reverse
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
