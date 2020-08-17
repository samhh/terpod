module Podcast (FeedId, EpisodeId, Episode (..), getPod) where

import Control.Lens ((^.))
import Data.Functor.Custom ((<$<))
import qualified Network.Wreq as R
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed)

type FeedId = Text

type EpisodeId = Text

data Episode = Episode
  { title :: Text,
    episodeUrl :: Text
  }

getPod :: String -> IO (Maybe Feed)
getPod = parseFeedSource . (^. R.responseBody) <$< R.get
