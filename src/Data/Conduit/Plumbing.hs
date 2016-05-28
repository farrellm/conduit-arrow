{-# LANGUAGE FlexibleContexts #-}

module Data.Conduit.Plumbing
       ( fanoutWith
       , fanout
       , split
       , feedback
       , scanl0
       , scanl0M
       ) where

import Prelude hiding (zip, zipWith)

import           Data.Conduit
import           Data.Conduit.Lift
import           Data.Conduit.Internal (ConduitM(..), Pipe(..), injectLeftovers)
import           Data.Void

import           Control.Monad.State.Lazy
import           Data.Sequence hiding (zip, zipWith, replicate)


-- derived from conduit 1.2.6.4; Data.Conduit.Internal.Conduit.zipConduitApp
fanoutWith :: Monad m => (b -> c -> d) -> Conduit a m b -> Conduit a m c -> Conduit a m d
fanoutWith f (ConduitM left0) (ConduitM right0) = ConduitM $ \rest -> let
    go _ _ (Done ()) (Done ()) = rest ()
    go finalX finalY (PipeM mx) y = PipeM (flip (go finalX finalY) y <$> mx)
    go finalX finalY x (PipeM my) = PipeM (go finalX finalY x <$> my)
    go _ _ (HaveOutput x finalX oX) (HaveOutput y finalY oY) = HaveOutput
        (go finalX finalY x y)
        (return ())
        (f oX oY)
    go _ finalY (HaveOutput x finalX _) y = go finalX finalY x y
    go finalX _ x (HaveOutput y finalY _) = go finalX finalY x y
    go _ _ (Leftover _ i) _ = absurd i
    go _ _ _ (Leftover _ i) = absurd i
    go finalX finalY (NeedInput px cx) (NeedInput py cy) = NeedInput
        (\i -> go finalX finalY (px i) (py i))
        (\u -> go finalX finalY (cx u) (cy u))
    go finalX finalY (NeedInput px cx) (Done y) = NeedInput
        (\i -> go finalX finalY (px i) (Done y))
        (\u -> go finalX finalY (cx u) (Done y))
    go finalX finalY (Done x) (NeedInput py cy) = NeedInput
        (\i -> go finalX finalY (Done x) (py i))
        (\u -> go finalX finalY (Done x) (cy u))
  in go (return ()) (return ()) (injectLeftovers $ left0 Done) (injectLeftovers $ right0 Done)

fanout :: Monad m => Conduit a m b -> Conduit a m c -> Conduit a m (b, c)
fanout = fanoutWith (\a b -> (a, b))


split :: Monad m => Conduit a m c -> Conduit b m d -> Conduit (a, b) m (c, d)
split (ConduitM left0) (ConduitM right0) = ConduitM $ \rest -> let
    go _ _ (Done ()) (Done ()) = rest ()
    go finalX finalY (PipeM mx) y = PipeM (flip (go finalX finalY) y <$> mx)
    go finalX finalY x (PipeM my) = PipeM (go finalX finalY x <$> my)
    go _ _ (HaveOutput x finalX oX) (HaveOutput y finalY oY) = HaveOutput
        (go finalX finalY x y)
        (return ())
        (oX, oY)
    go _ finalY (HaveOutput x finalX _) y = go finalX finalY x y
    go finalX _ x (HaveOutput y finalY _) = go finalX finalY x y
    go _ _ (Leftover _ i) _ = absurd i
    go _ _ _ (Leftover _ i) = absurd i
    go finalX finalY (NeedInput px cx) (NeedInput py cy) = NeedInput
        (\(l, r) -> go finalX finalY (px l) (py r))
        (\u -> go finalX finalY (cx u) (cy u))
    go finalX finalY (NeedInput px cx) (Done y) = NeedInput
        (\(l, _) -> go finalX finalY (px l) (Done y))
        (\u -> go finalX finalY (cx u) (Done y))
    go finalX finalY (Done x) (NeedInput py cy) = NeedInput
        (\(_, r) -> go finalX finalY (Done x) (py r))
        (\u -> go finalX finalY (Done x) (cy u))
  in go (return ()) (return ()) (injectLeftovers $ left0 Done) (injectLeftovers $ right0 Done)


feedback :: Monad m => Conduit (a, c) m (b, c) -> c -> Conduit a m b
feedback c n = evalStateLC (singleton n) . loop $ transPipe lift c
  where loop (ConduitM p) =  ConduitM $ \rest -> let
          go _ (Done ()) = rest ()

          go _ (HaveOutput x finalX o) = PipeM $ do
            modify (|> snd o)
            pure (HaveOutput
                  (go finalX x)
                  finalX
                  (fst o))

          go _ (Leftover _ i) = absurd i

          go finalX (PipeM mx) = PipeM (go finalX <$> mx)

          go finalX (NeedInput px cx) = PipeM $ do
            (l :< ls) <- viewl <$> get
            put ls
            pure (NeedInput
                  (\i -> go finalX (px (i, l)))
                  (go finalX . cx))

          in go (return ()) (injectLeftovers $ p Done)

-- taken from conduit-combinators-1.0.3.1, Data.Conduit.Combinators.scanl
scanl0 :: Monad m => (a -> b -> a) -> a -> Conduit b m a
scanl0 f = _loop
  where _loop seed = await >>= maybe (pure ()) go
          where go b =
                  do let seed' = f seed b
                     seed' `seq` yield seed'
                     _loop seed'

-- taken from conduit-combinators-1.0.3.1, Data.Conduit.Combinators.scanlM
scanl0M :: Monad m => (a -> b -> m a) -> a -> Conduit b m a
scanl0M f = _loop
  where _loop seed = await >>= maybe (pure ()) go
          where go b =
                  do seed' <- lift $ f seed b
                     seed' `seq` yield seed'
                     _loop seed'
