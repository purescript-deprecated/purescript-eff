module Control.Monad.Eff.Unsafe where

import Control.Monad.Eff (Eff)

-- | Run an effectful computation.
-- |
-- | *Note*: use of this function can result in arbitrary side-effects.
foreign import unsafePerformEff :: forall a. Eff a -> a
