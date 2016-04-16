module Data.Conduit.Arrow.Internal
       ( ConduitArrow(..)
       ) where

import           Prelude hiding ((.))
import           Control.Category
import           Control.Arrow
import           Data.Conduit
import qualified Data.Conduit.Combinators as CC
import           Data.Either

newtype ConduitArrow m i o = ConduitArrow {getConduit :: Conduit i m o}
splitC :: Monad m => Conduit (Maybe (Either a b)) m [Either a b]
splitC = s []
  where s ls = do
          ml <- await
          case ml of
            Nothing -> return ()
            Just Nothing -> do
              yield $ reverse ls
              s []
            Just (Just l) -> s $ l:ls

instance (Monad m) => Category (ConduitArrow m) where
  id = ConduitArrow $ awaitForever yield
  (.) (ConduitArrow c1) (ConduitArrow c2) = ConduitArrow (c2 =$= c1)

instance Monad m => Arrow (ConduitArrow m) where
  arr f = ConduitArrow . awaitForever $ yield . f

  first (ConduitArrow c) = ConduitArrow $
    getZipConduit (ZipConduit (CC.map fst =$= c =$= CC.map (Just . Left)) <*
                   ZipConduit (CC.map snd =$= CC.map (Just . Right)) <*
                   ZipConduit (CC.map $ const Nothing)) =$=
    splitC =$=
    CC.map partitionEithers =$=
    CC.concatMap (uncurry zip)

  -- second (ConduitArrow c) = ConduitArrow $
  -- (&&&) (ConduitArrow c1) (ConduitArrow c2) = ConduitArrow $
  -- (***) (ConduitArrow c1) (ConduitArrow c2) = ConduitArrow $
