{-# LANGUAGE FlexibleInstances #-}

module Data.Conduit.Arrow
       (getConduit
       , ConduitArrow
       , (=$>>)
       , (>>$=)
       , (>>$$)
       , map
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
import qualified Data.Conduit.Combinators as CC
import           Data.MonoTraversable

newtype ConduitArrow m i o = ConduitArrow {getConduit :: Conduit i m o}

instance (Monad m) => Category (ConduitArrow m) where
  id = ConduitArrow $ awaitForever yield
  (.) (ConduitArrow c1) (ConduitArrow c2) = ConduitArrow (c2 =$= c1)

instance Monad m => Arrow (ConduitArrow m) where
  arr f = ConduitArrow . awaitForever $ yield . f

  first (ConduitArrow c) = ConduitArrow $
    awaitForever (\i -> do
                     o <- yield (fst i) =$= c =$= CC.sinkList
                     CC.yieldMany $ zip o [snd i])

  second (ConduitArrow c) = ConduitArrow $
    awaitForever (\i -> do
                     o <- yield (snd i) =$= c =$= CC.sinkList
                     CC.yieldMany $ zip [fst i] o)

  (&&&) (ConduitArrow c1) (ConduitArrow c2) = ConduitArrow $
    awaitForever (\i -> do
                     o1 <- yield i =$= c1 =$= CC.sinkList
                     o2 <- yield i =$= c2 =$= CC.sinkList
                     CC.yieldMany $ zip o1 o2)

  (***) (ConduitArrow c1) (ConduitArrow c2) = ConduitArrow $
    awaitForever (\i -> do
                     o1 <- yield (fst i) =$= c1 =$= CC.sinkList
                     o2 <- yield (snd i) =$= c2 =$= CC.sinkList
                     CC.yieldMany $ zip o1 o2)

infixr 1 =$>>
(=$>>) :: Monad m => Conduit i m b -> ConduitArrow m b o -> ConduitArrow m i o
(=$>>) s (ConduitArrow c) = ConduitArrow $ s =$= c

infixr 1 >>$=
(>>$=) :: Monad m => ConduitArrow m i b -> ConduitM b o m () -> ConduitArrow m i o
(>>$=) (ConduitArrow c) s = ConduitArrow $ c =$= s

infixr 0 >>$$
(>>$$) :: Monad m => ConduitArrow m () a -> Sink a m b -> m b
(>>$$) (ConduitArrow c) s = c $$ s

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
