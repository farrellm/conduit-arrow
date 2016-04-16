module Data.Conduit.Arrow
       (getConduit
       , ConduitArrow()
       , (=$>>)
       , (>>$=)
       , (>>$$)
       ) where

import           Data.Conduit
import           Data.Conduit.Arrow.Internal

infixr 1 =$>>
(=$>>) :: Monad m => Source m a -> ConduitArrow m a b -> Conduit () m b
(=$>>) s (ConduitArrow c) = s =$= c

infixr 1 >>$=
(>>$=) :: Monad m => ConduitArrow m i b -> ConduitM b o m () -> ConduitArrow m i o
(>>$=) (ConduitArrow c) s = ConduitArrow $ c =$= s

infixr 0 >>$$
(>>$$) :: Monad m => ConduitArrow m () a -> Sink a m b -> m b
(>>$$) (ConduitArrow c) s = c $$ s
