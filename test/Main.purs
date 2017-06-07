module Test.Main where

import Prelude
import Control.Monad.Eff (Eff, foreachE, traverseE)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  let foobar = ["foo", "bar", "bam"]
  foreachE foobar log
  traverseE foobar log
