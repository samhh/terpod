module Data.String.Custom (surround, withNewline) where

surround :: IsString a => Semigroup a => a -> a -> a
surround outside inside = outside <> inside <> outside

withNewline :: IsString a => Semigroup a => a -> a -> a
withNewline a b = a <> "\n" <> b
