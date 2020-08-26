module Podcast (PodcastId (PodcastId), _KeyPodcastId, getPodcast) where

import Control.Lens ((^.))
import Control.Newtype.Generics (Newtype, pack, unpack)
import Data.Functor.Custom ((<$<))
import qualified Network.Wreq as R
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed)
import Toml (Key, TomlBiMap)
import Toml.Codec.BiMap.Conversion.Custom (textBiMap)

newtype PodcastId = PodcastId Text
  deriving (Show, Eq, Ord, Generic)

instance Newtype PodcastId

_KeyPodcastId :: TomlBiMap Key PodcastId
_KeyPodcastId = textBiMap pack unpack

getPodcast :: String -> IO (Maybe Feed)
getPodcast = parseFeedSource . (^. R.responseBody) <$< R.get
