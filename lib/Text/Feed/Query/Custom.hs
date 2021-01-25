module Text.Feed.Query.Custom (getItemEnclosureLink, getItemId') where

import           Data.Functor.Custom ((<$<))
import           Data.Tuple.Custom   (fst3)
import           Text.Feed.Query     (ItemGetter, getItemEnclosure, getItemId)

getItemId' :: ItemGetter Text
getItemId' = snd <$< getItemId

getItemEnclosureLink :: ItemGetter Text
getItemEnclosureLink = fst3 <$< getItemEnclosure
