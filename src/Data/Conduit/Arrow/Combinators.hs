module Data.Conduit.Arrow.Combinators
       ( map
       , mapE
       , omapE
       , scanl
       , mapM
       , mapME
       , omapME
       , iterM
       , scanlM
       ) where

import Prelude hiding ((.), map, scanl, mapM)

import           Control.Category
import           Control.Arrow
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
