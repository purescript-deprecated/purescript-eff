module Bench.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Traversable (for_, intercalate)
import Performance.Minibench (BenchResult, benchWith', withUnits)


type BenchEff = (console :: CONSOLE)

testApply :: forall m. MonadEff BenchEff m => Int -> m Unit
testApply n' = do
  arr <- liftEff mkArr
  applyLoop (void <<< liftEff <<< pushToArr arr) n'
  where
  applyLoop :: Monad m => (Int -> m Unit) -> Int -> m Unit
  applyLoop eff max = go (pure unit) 0
    where 
    go acc n | n == max = acc
    go acc n = go (acc <* eff n) (n + 1)


testBindRight :: forall m. MonadEff BenchEff m => Int -> m Unit
testBindRight n' = do
  arr <- liftEff mkArr
  bindRightLoop (void <<< liftEff <<< pushToArr arr) n'
  where
  bindRightLoop :: Monad m => (Int -> m Unit)  -> Int -> m Unit
  bindRightLoop eff max = go (pure unit) 0
    where 
    go acc n | n == max = acc
    go acc n = go (eff (max - n - 1) >>= const acc) (n + 1)


testBindLeft :: forall m. MonadEff BenchEff m => Int -> m Unit
testBindLeft n' = do
  arr <- liftEff mkArr
  bindLeftLoop (void <<< liftEff <<< pushToArr arr) n'
  where
  bindLeftLoop :: Monad m => (Int -> m Unit)  -> Int -> m Unit
  bindLeftLoop eff max = go (pure unit) 0
    where 
    go acc n | n == max = acc
    go acc n = go (acc >>= const (eff n)) (n + 1)


testMap :: forall m. MonadEff BenchEff m => Int -> m Unit
testMap n = do
  arr <- liftEff mkArr
  res <- mapLoop n (liftEff $ pushToArr arr 0)
  pure unit
  where
  mapLoop :: Monad m => Int -> m Int -> m Int
  mapLoop max i = 
    if max == 0 
      then i 
      else mapLoop (max - 1) (map (_ + 1) i)


main :: Eff BenchEff Unit
main = do
  log "| bench | type | n | mean | stddev | min | max |"
  log "| ----- | ---- | - | ---- | ------ | --- | --- |"
  bench 10 ">>=R" testBindRight testBindRight [100, 1000, 5000]
  bench 10 ">>=L" testBindLeft testBindLeft [100, 1000, 5000]
  bench 10 "map" testMap testMap [100, 1000, 5000]
  bench 10 "apply" testApply testApply [100, 1000, 5000]
  log "| - | - | - | - | - | - | - |"
  bench 2 ">>=R" testBindRight testBindRight [10000, 50000, 100000, 1000000]
  bench 2 ">>=L" testBindLeft testBindLeft [10000, 50000, 100000, 1000000]
  bench 2 "map" testMap testMap [10000, 50000, 100000, 1000000, 350000, 700000]
  bench 2 "apply" testApply testApply [10000, 50000, 100000, 1000000]

bench
  :: Int
  -> String
  -> (Int -> Eff BenchEff Unit)
  -> (Int -> Aff BenchEff Unit)
  -> Array Int
  -> Eff BenchEff Unit
bench n name buildEff buildAff vals = for_ vals \val -> do 
  logBench [name <> " build", "Eff", show val] $ benchWith' n \_ -> buildEff val
  logBench' id [name <> " build", "Aff", show val] $ benchWith' n \_ -> buildAff val
  let eff = liftEff $ buildEff val
  logBench [name <> " run", "Eff", show val] $ benchWith' n \_ -> unsafePerformEff eff
  let aff = launchAff_ $ buildAff val
  logBench' id [name <> " run", "Aff", show val] $ benchWith' n \_ -> unsafePerformEff aff

logBench' :: (String -> String) -> Array String -> Eff BenchEff BenchResult -> Eff BenchEff Unit
logBench' f msg benchEff = do
  res <- benchEff
  let 
    logStr = intercalate " | " 
      $ append msg 
      $ map (f <<< withUnits) [res.mean, res.stdDev, res.min, res.max]
  log $  "| "  <> logStr <>  " |"

logBench :: Array String -> Eff BenchEff BenchResult -> Eff BenchEff Unit
logBench = logBench' \s -> "**" <> s <> "**"

foreign import data Arr :: Type -> Type
foreign import mkArr :: forall e a. Eff e (Arr a)
foreign import pushToArr :: forall e a. Arr a -> a -> Eff e a
foreign import log :: forall e a. a -> Eff e Unit

