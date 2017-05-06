module Control.Monad.Eff.Unrefined
  ( Eff
  , unrefined
  , runEff
  , untilE
  , whileE
  , forE
  , foreachE
  ) where

import Control.Monad.Eff as Refined
import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Function ((<<<))
import Data.Functor (class Functor)
import Data.Unit (Unit)

foreign import data UNREFINED :: # Refined.Effect

-- | A variant of `Control.Monad.Eff.Eff` without the row of effects.
-- |
-- | `Unrefined.Eff a` is isomorphic to `exists eff. Refined.Eff a`.
-- | This variant can be useful sometimes when we are not concerned with the
-- | particular set of effects. Unrefined effectful computations can be used
-- | in `main`, just like refined effects, since they share the same representation.
newtype Eff a = Eff (Refined.Eff UNREFINED a)

derive newtype instance functorEff :: Functor Eff
derive newtype instance applyEff :: Apply Eff
derive newtype instance applicativeEff :: Applicative Eff
derive newtype instance bindEff :: Bind Eff
derive newtype instance monadEff :: Monad Eff

-- | Convert a refined effectful computation into an unrefined one, losing effect
-- | row information.
unrefined :: forall eff a. Refined.Eff eff a -> Eff a
unrefined = Eff <<< unsafeCoerceEff

refined :: forall a. Eff a -> Refined.Eff UNREFINED a
refined (Eff u) = u

-- | Unpack a computation of type `Eff a` using a fresh effect row.
-- |
-- | _Note_: it should not be necessary to use this function in normal
-- | circumstances, since the unrefined `Eff` type can be used in `main`.
-- | However, it is provided for compatibility with APIs which use refined
-- | effects.
runEff :: forall a r. Eff a -> (forall eff. Refined.Eff eff a -> r) -> r
runEff (Eff u) f = f u

-- | Loop until a condition becomes `true`.
-- |
-- | `untilE b` is an effectful computation which repeatedly runs the effectful
-- | computation `b`, until its return value is `true`.
untilE :: Eff Boolean -> Eff Unit
untilE cond = Eff (Refined.untilE (refined cond))

-- | Loop while a condition is `true`.
-- |
-- | `whileE b m` is effectful computation which runs the effectful computation
-- | `b`. If its result is `true`, it runs the effectful computation `m` and
-- | loops. If not, the computation ends.
whileE :: forall a. Eff Boolean -> Eff a -> Eff Unit
whileE cond e = Eff (Refined.whileE (refined cond) (refined e))

-- | Loop over a consecutive collection of numbers.
-- |
-- | `forE lo hi f` runs the computation returned by the function `f` for each
-- | of the inputs between `lo` (inclusive) and `hi` (exclusive).
forE :: Int -> Int -> (Int -> Eff Unit) -> Eff Unit
forE lo hi f = Eff (Refined.forE lo hi (refined <<< f))

-- | Loop over an array of values.
-- |
-- | `foreach xs f` runs the computation returned by the function `f` for each
-- | of the inputs `xs`.
foreachE :: forall a. Array a -> (a -> Eff Unit) -> Eff Unit
foreachE xs f = Eff (Refined.foreachE xs (refined <<< f))
