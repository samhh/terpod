module Podcast (PodcastId (PodcastId), unPodcastId, _KeyPodcastId, getPodcast) where

import Control.Lens ((^.))
import Data.Functor.Custom ((<$<))
import Data.Text (pack)
import qualified Network.Wreq as R
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed)
import Toml (Key, TomlBiMap)
import Toml.Codec.BiMap.Conversion.Custom (textBiMap)

newtype PodcastId = PodcastId Text
  deriving (Show, Eq, Ord)

unPodcastId :: PodcastId -> Text
unPodcastId (PodcastId x) = x

instance IsString PodcastId where
  fromString = PodcastId . pack

instance Semigroup PodcastId where
  x <> y = PodcastId $ (unPodcastId x) <> (unPodcastId y)

_KeyPodcastId :: TomlBiMap Key PodcastId
_KeyPodcastId = textBiMap PodcastId unPodcastId

getPodcast :: String -> IO (Maybe Feed)
getPodcast = parseFeedSource . (^. R.responseBody) <$< R.get
