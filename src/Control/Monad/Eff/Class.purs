module Control.Monad.Eff.Class where

import Control.Category (id)
import Control.Monad (class Monad)
import Control.Monad.Eff (Eff)

-- | The `MonadEff` class captures those monads which support native effects.
-- |
-- | Instances are provided for `Eff` itself, and the standard monad
-- | transformers.
-- |
-- | `liftEff` can be used in any appropriate monad transformer stack to lift an
-- | action of type `Eff a` into the monad.
class Monad m <= MonadEff m where
  liftEff :: forall a. Eff a -> m a

instance monadEffEff :: MonadEff Eff where
  liftEff = id
