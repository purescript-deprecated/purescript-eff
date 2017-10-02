module Control.Monad.Eff
  ( Eff
  , untilE
  , whileE
  , forE
  , foreachE
  ) where

import Control.Applicative (class Applicative, liftA1)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad, ap)
import Data.Functor (class Functor)
import Data.Unit (Unit)

-- | The `Eff` type constructor is used to represent _native_ effects.
-- |
-- | The type parameter is the return type of the computation.
foreign import data Eff :: Type -> Type

instance functorEff :: Functor Eff where
  map = liftA1

instance applyEff :: Apply Eff where
  apply = ap

instance applicativeEff :: Applicative Eff where
  pure = pureE

foreign import pureE :: forall a. a -> Eff a

instance bindEff :: Bind Eff where
  bind = bindE

foreign import bindE :: forall a b. Eff a -> (a -> Eff b) -> Eff b

instance monadEff :: Monad Eff

-- | Loop until a condition becomes `true`.
-- |
-- | `untilE b` is an effectful computation which repeatedly runs the effectful
-- | computation `b`, until its return value is `true`.
foreign import untilE :: Eff Boolean -> Eff Unit

-- | Loop while a condition is `true`.
-- |
-- | `whileE b m` is effectful computation which runs the effectful computation
-- | `b`. If its result is `true`, it runs the effectful computation `m` and
-- | loops. If not, the computation ends.
foreign import whileE :: forall a. Eff Boolean -> Eff a -> Eff Unit

-- | Loop over a consecutive collection of numbers.
-- |
-- | `forE lo hi f` runs the computation returned by the function `f` for each
-- | of the inputs between `lo` (inclusive) and `hi` (exclusive).
foreign import forE :: Int -> Int -> (Int -> Eff Unit) -> Eff Unit

-- | Loop over an array of values.
-- |
-- | `foreachE xs f` runs the computation returned by the function `f` for each
-- | of the inputs `xs`.
foreign import foreachE :: forall a. Array a -> (a -> Eff Unit) -> Eff Unit
