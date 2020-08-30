module Podcast (PodcastId (PodcastId), _KeyPodcastId, getPodcast) where

import Control.Exception (try)
import Control.Lens ((^.))
import Control.Newtype.Generics (Newtype, pack, unpack)
import Data.Functor.Custom ((<$<))
import Network.HTTP.Client (HttpException)
import qualified Network.Wreq as R
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed)
import Toml (Key, TomlBiMap)
import Toml.Codec.BiMap.Conversion.Custom (textBiMap)

newtype PodcastId = PodcastId Text
  deriving (Show, Eq, Ord, Generic, IsString)

instance Newtype PodcastId

_KeyPodcastId :: TomlBiMap Key PodcastId
_KeyPodcastId = textBiMap pack unpack

-- | Given a URL, attempt to retrieve a feed and parse it. `Left` represents
-- a network-level failure and `Nothing` represents a failure to decode the
-- response body as a feed.
getPodcast :: String -> IO (Either HttpException (Maybe Feed))
getPodcast = fmap (parseFeedSource . (^. R.responseBody)) <$< try . R.get
