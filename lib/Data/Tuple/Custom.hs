module Data.Tuple.Custom (dup, fst3) where

dup :: a -> (a, a)
dup x = (x, x)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

