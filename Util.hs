
module Util where

import Control.Applicative

-- Wraps the value in a minimal context if the predicate is true, otherwise
-- returns the empty value
wrapIf :: (Alternative m, Monad m) => (a -> Bool) -> a -> m a
wrapIf p v = if p v then return v else empty
