module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Apply (lift2)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Console (CONSOLE)


testLift2 :: Eff () Unit
testLift2 = do
  arr <- mkArr
  res <- (pushToArr arr 1) `lift2 (+)` (pushToArr arr 2)
  res' <- (pure 1) `lift2 (+)` (pure 2)
  assert ([1, 2] == unArr arr) "lift2 1/3"
  assert (3 == res') "lift2 2/3"
  assert (3 == res) "lift2 3/3"


testApply :: Int -> Eff () Unit
testApply n' = do
  arr <-  mkArr
  applyLoop (void <<< pushToArr arr) n'
  assert (naturals n' == unArr arr) $ "apply " <> show n'
  where
  applyLoop eff max = go (pure unit) 0
    where 
    go acc n | n == max = acc
    go acc n = go (acc <* eff n) (n + 1)



testBindRight :: Int -> Eff () Unit
testBindRight n' = do
  arr <-  mkArr
  bindRightLoop (void <<< pushToArr arr) n'
  assert (naturals n' == unArr arr) $ "bind right " <> show n'
  where
  bindRightLoop eff max = go (pure unit) 0
    where 
    go acc n | n == max = acc
    go acc n = go (eff (max - n - 1) >>= const acc) (n + 1)


testBindLeft :: Int -> Eff () Unit
testBindLeft n' = do
  arr <-  mkArr
  bindLeftLoop (void <<< pushToArr arr) n'
  assert (naturals n' == unArr arr) $ "bind left " <> show n'
  where
  bindLeftLoop eff max = go (pure unit) 0
    where 
    go acc n | n == max = acc
    go acc n = go (acc >>= const (eff n)) (n + 1)


testMap :: Int -> Eff () Unit
testMap n = do
  arr <- mkArr
  res <- mapLoop n (pushToArr arr 0)
  assert (res == n) $ "map " <> show n
  assert ([0] == unArr arr) $ "map" 
  where
  mapLoop max i = 
    if max == 0 
      then i 
      else mapLoop (max - 1) (map (_ + 1) i)


main :: Eff () Unit
main = do
  test "testLift2" $ testLift2
  test "testBindRight" $ testBindRight 1000000
  test "testBindLeft" $ testMap 1000000
  test "testMap" $ testMap 5000000
  test "testApply" $ testApply 1000000
  where
  test msg eff = do
    time msg
    eff
    timeEnd msg


foreign import data Arr :: Type -> Type


foreign import mkArr :: forall e a. Eff e (Arr a)
foreign import pushToArr :: forall e a. Arr a -> a -> Eff e a
foreign import assert :: forall e. Boolean -> String -> Eff e Unit
foreign import log :: forall e a. a -> Eff e Unit
foreign import unArr :: forall a. Arr a -> Array a
foreign import naturals :: Int -> Array Int

foreign import time :: forall e. String -> Eff e Unit
foreign import timeEnd :: forall e. String -> Eff e Unit
