{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module MonadToJoin where

import Block2Monads (Monad(..), MonadJoin(..))
import Prelude hiding (Monad, (>>=), return)

instance Monad m => MonadJoin m where
	returnJoin = return
	join m = m >>= id
