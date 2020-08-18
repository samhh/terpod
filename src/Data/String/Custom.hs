module Data.String.Custom (surround, unsurround) where

import RIO.Text (dropPrefix, dropSuffix)

surround :: Semigroup a => a -> a -> a
surround outside inside = outside <> inside <> outside

unsurround :: Text -> Text -> Text
unsurround around = dropPrefix around . dropSuffix around
