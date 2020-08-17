module Episode (EpisodeId (EpisodeId), Episode (..), unEpisodeId, _KeyEpisodeId) where

import Data.Text (pack)
import Toml (Key, TomlBiMap)
import Toml.Codec.BiMap.Conversion.Custom (textBiMap)

newtype EpisodeId = EpisodeId Text
  deriving (Show, Eq, Ord)

unEpisodeId :: EpisodeId -> Text
unEpisodeId (EpisodeId x) = x

instance IsString EpisodeId where
  fromString = EpisodeId . pack

instance Semigroup EpisodeId where
  a <> b = a <> b

_KeyEpisodeId :: TomlBiMap Key EpisodeId
_KeyEpisodeId = textBiMap EpisodeId unEpisodeId

data Episode = Episode
  { title :: Text,
    episodeUrl :: Text
  }
