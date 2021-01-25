module Data.Functor.Custom ((<$<)) where

-- | Compose two functions where the first returns a functor and the second is
-- | to be applied within said functor.
(<$<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<$<) = fmap . fmap

infixr 8 <$<
