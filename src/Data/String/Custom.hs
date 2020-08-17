module Data.String.Custom (surround) where

surround :: IsString a => Semigroup a => a -> a -> a
surround outside inside = outside <> inside <> outside
