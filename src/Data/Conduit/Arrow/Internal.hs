module Data.Conduit.Arrow.Internal
       ( ConduitArrow(..)
       ) where

import           Prelude hiding ((.))
import           Control.Category
import           Control.Arrow
import           Data.Conduit
import qualified Data.Conduit.Combinators as CC
import           Data.Either

zipFirstC :: Monad m => Conduit a m b -> Conduit (a, b1) m (Maybe (Either b b1))
zipFirstC c = getZipConduit (ZipConduit (CC.map fst =$= c =$= CC.map (Just . Left)) <*
                             ZipConduit (CC.map snd =$= CC.map (Just . Right)) <*
                             ZipConduit (CC.mapM (const $ pure Nothing)))

newtype ConduitArrow m i o = ConduitArrow {getConduit :: Conduit i m o}

instance (Monad m) => Category (ConduitArrow m) where
  id = ConduitArrow $ awaitForever yield
  (.) (ConduitArrow c1) (ConduitArrow c2) = ConduitArrow (c2 =$= c1)

instance Monad m => Arrow (ConduitArrow m) where
  arr f = ConduitArrow . awaitForever $ yield . f

  first (ConduitArrow c) = ConduitArrow $
    zipFirstC c =$=
    split [] =$=
    CC.map partitionEithers =$=
    CC.concatMap (uncurry zip)
    where split ls = do
            ml <- await
            case ml of
              Nothing       -> return ()
              Just Nothing  -> yield (reverse ls) >> split []
              Just (Just l) -> split $ l:ls

  -- second (ConduitArrow c) = ConduitArrow $
  -- (&&&) (ConduitArrow c1) (ConduitArrow c2) = ConduitArrow $
  -- (***) (ConduitArrow c1) (ConduitArrow c2) = ConduitArrow $


instance Monad m => ArrowZero (ConduitArrow m) where
  zeroArrow = ConduitArrow $ pure ()

instance Monad m => ArrowPlus (ConduitArrow m) where
  (<+>) (ConduitArrow a) (ConduitArrow b) = ConduitArrow $
    getZipConduit (ZipConduit a <* ZipConduit b)


instance Monad m => ArrowChoice (ConduitArrow m) where
  left (ConduitArrow a) = ConduitArrow $
    getZipConduit
    ( ZipConduit (CC.filter isLeft =$= CC.map getLeft =$= a =$= CC.map Left) <*
      ZipConduit (CC.filter isRight =$= CC.map (Right .getRight)))
    where getLeft (Left v) = v
          getRight (Right v) = v
