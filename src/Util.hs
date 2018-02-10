
module Util where

import Protolude

-- Wraps the value in a minimal context if the predicate is true, otherwise
-- returns the empty value
wrapIf :: (Alternative m, Monad m) => (a -> Bool) -> a -> m a
wrapIf p v = if p v then return v else empty

replace :: Eq a => (a -> Bool) -> a -> [a] -> [a]
replace p v' = map (\x -> if p x then v' else x)

