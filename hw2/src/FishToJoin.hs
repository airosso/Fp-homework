{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module FishToJoin where

import Block2Monads (MonadFish(..), MonadJoin(..))

instance MonadFish m => MonadJoin m where
	returnJoin = returnFish
	join = id >=> id
