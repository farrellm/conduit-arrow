# conduit-arrow
Quasi-Arrow instance for Conduit

## Conduits aren't Arrows!

At first blush, conduits look a lot like an instance of an arrow.
Both are build on composition, with the conduit combinator `=$=`
acting like `>>>`.  But on closer inspection, there is a fundamental
problem.  Namely, the conduit type signature is a lie.

```haskell
type Conduit i m o = ConduitM i o m ()
```

One is led to believe a conduit maps an `i` to an `o`, but this is not
strictly true.  Consider,

```haskell
import           Data.Conduit
import qualified Data.Conduit.Combinators as CC

blackhole :: Monad m => Conduit i m o
blackhole = awaitForever (\i -> CC.yieldMany [])

whitehole :: Monad m => (i -> o) -> Conduit i m o
whitehole f = awaitForever (\i -> CC.yieldMany . repeat $ f i)
```

What happened?  These are both bad arrows, with either `blackhole` or
`whitehole` (or both) causing your Arrow instance to break type class
laws.  Fundamentally, the problem is conduits don't map `a -> b`, they
map `a -> [b]`, then silently concatinate the `[b]` before forwarding
them to the next stage.

### but…

Perhapse we can be a bit more… nuanced with our definitions.  Rather
than taking the full class of all possible condits, we restrict our
attention to the class of conduits that really do map `a -> b`, which
make a perfectly nice arrow instance.

## Examples

TODO
