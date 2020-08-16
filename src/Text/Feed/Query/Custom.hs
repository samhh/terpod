module Text.Feed.Query.Custom (getItemId') where

import Data.Functor.Custom ((<$<))
import Text.Feed.Query (ItemGetter, getItemId)

getItemId' :: ItemGetter Text
getItemId' = snd <$< getItemId

