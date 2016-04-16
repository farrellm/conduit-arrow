module Data.Conduit.Arrow.Combinators
       ( map
       , mapE
       , omapE
       , scanl
       , scanl0
       , mapM
       , mapME
       , omapME
       , iterM
       , scanlM
       , scanl0M
       ) where

import Prelude hiding (map, scanl, mapM)

import           Control.Monad.Trans.Class
import           Data.Conduit
import           Data.Conduit.Arrow.Internal
import qualified Data.Conduit.Combinators as CC
import           Data.MonoTraversable

map :: Monad m => (a -> b) -> ConduitArrow m a b
map = ConduitArrow . CC.map

mapE :: (Monad m, Functor f) => (a -> b) -> ConduitArrow m (f a) (f b)
mapE = ConduitArrow . CC.mapE

omapE :: (Monad m, MonoFunctor mono) => (Element mono -> Element mono) -> ConduitArrow m mono mono
omapE = ConduitArrow . CC.omapE

scanl :: Monad m => (a -> b -> a) -> a -> ConduitArrow m b a
scanl f a = ConduitArrow $ CC.scanl f a

-- taken from conduit-combinators-1.0.3.1, Data.Conduit.Combinators.scanl
scanl0C :: Monad m => (a -> b -> a) -> a -> Conduit b m a
scanl0C f =
    _loop
  where
    _loop seed =
        await >>= maybe (pure ()) go
      where
        go b = do
            let seed' = f seed b
            seed' `seq` yield seed'
            _loop seed'

scanl0 :: Monad m => (a -> b -> a) -> a -> ConduitArrow m  b a
scanl0 f = ConduitArrow . scanl0C f

mapM :: Monad m => (a -> m b) -> ConduitArrow m a b
mapM = ConduitArrow . CC.mapM

mapME :: (Monad m, Traversable f) => (a -> m b) -> ConduitArrow m (f a) (f b)
mapME = ConduitArrow . CC.mapME

omapME :: (Monad m, MonoTraversable mono) =>
          (Element mono -> m (Element mono)) -> ConduitArrow m mono mono
omapME = ConduitArrow . CC.omapME

iterM :: Monad m => (a -> m ()) -> ConduitArrow m a a
iterM = ConduitArrow . CC.iterM

scanlM :: Monad m => (a -> b -> m a) -> a -> ConduitArrow m b a
scanlM f a = ConduitArrow $ CC.scanlM f a

-- taken from conduit-combinators-1.0.3.1, Data.Conduit.Combinators.scanlM
scanl0MC :: Monad m => (a -> b -> m a) -> a -> Conduit b m a
scanl0MC f =
    _loop
  where
    _loop seed =
        await >>= maybe (pure ()) go
      where
        go b = do
            seed' <- lift $ f seed b
            seed' `seq` yield seed'
            _loop seed'

scanl0M :: Monad m => (a -> b -> m a) -> a -> ConduitArrow m  b a
scanl0M f = ConduitArrow . scanl0MC f
