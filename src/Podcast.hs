module Podcast (PodcastId, getPodcast) where

import Control.Lens ((^.))
import Data.Functor.Custom ((<$<))
import qualified Network.Wreq as R
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed)

type PodcastId = Text

getPodcast :: String -> IO (Maybe Feed)
getPodcast = parseFeedSource . (^. R.responseBody) <$< R.get
