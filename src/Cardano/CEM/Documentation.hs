module Cardano.CEM.Documentation (cemDotGraphString) where

import Prelude

import Data.Foldable (fold)
import Data.Map qualified as Map
import Data.Proxy

import Cardano.CEM

dotStyling :: String
dotStyling =
  "rankdir=LR;\n"
    <> "node [shape=\"dot\",fontsize=14,fixedsize=true,width=1.5];\n"
    <> "edge [fontsize=11];"
    <> "\"Void In\" [color=\"orange\"];"
    <> "\"Void Out\" [color=\"orange\"];"

cemDotGraphString :: (CEMScript script) => String -> Proxy script -> String
cemDotGraphString name proxy =
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
        [ maybe "\"Void In\"" showSpine from
          <> " -> "
          <> maybe "\"Void Out\"" showSpine to
          <> " [label=\""
          <> showSpine transition
          <> " (stage "
          <> show stage
          <> ")"
          <> "\"]; \n"
        | (transition, (stage, from, to)) <-
            Map.assocs $ transitionStage proxy
        ]
