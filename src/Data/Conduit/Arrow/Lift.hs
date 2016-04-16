{-# LANGUAGE FlexibleContexts #-}
module Data.Conduit.Arrow.Lift
       ( runReaderCA
       , evalStateLCA
       , evalStateCA
       -- , catchErrorCA
       ) where

import           Data.Conduit.Arrow.Internal
import           Data.Conduit.Lift
-- import           Control.Monad.Trans.Error
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.State.Lazy as L

runReaderCA :: Monad m => r -> ConduitArrow (ReaderT r m) i o -> ConduitArrow m i o
runReaderCA r (ConduitArrow c) = ConduitArrow $ runReaderC r c

evalStateLCA :: Monad m => s -> ConduitArrow (L.StateT s m) i o -> ConduitArrow m i o
evalStateLCA s (ConduitArrow c) = ConduitArrow $ evalStateLC s c

evalStateCA :: Monad m => s -> ConduitArrow (S.StateT s m) i o -> ConduitArrow m i o
evalStateCA s (ConduitArrow c) = ConduitArrow $ evalStateC s c

-- catchErrorCA
--   :: (Monad m, Error e) =>
--      ConduitArrow (ErrorT e m) i o -> (e -> ConduitArrow (ErrorT e m) i o) ->
--      ConduitArrow (ErrorT e m) i o
-- catchErrorCA (ConduitArrow c) f = ConduitArrow $ catchErrorC c $ getConduit . f
