module Episode (EpisodeId (EpisodeId), Episode (..), unEpisodeId, _KeyEpisodeId, downloadEpisode) where

import Config (DownloadPath, unDownloadPath)
import Control.Lens ((^.))
import Data.Functor.Custom ((<$<))
import Data.Text (pack, unpack)
import qualified Network.Wreq as R
import Podcast (PodcastId, unPodcastId)
import System.FilePath.Posix ((</>), takeExtension)
import Toml (Key, TomlBiMap)
import Toml.Codec.BiMap.Conversion.Custom (textBiMap)

newtype EpisodeId = EpisodeId Text
  deriving (Show, Eq, Ord)

unEpisodeId :: EpisodeId -> Text
unEpisodeId (EpisodeId x) = x

instance IsString EpisodeId where
  fromString = EpisodeId . pack

instance Semigroup EpisodeId where
  x <> y = EpisodeId $ unEpisodeId x <> unEpisodeId y

_KeyEpisodeId :: TomlBiMap Key EpisodeId
_KeyEpisodeId = textBiMap EpisodeId unEpisodeId

data Episode = Episode
  { title :: Text,
    episodeUrl :: Text
  }
  deriving (Show)

-- | Download a podcast episode onto the filesystem.
downloadEpisode :: DownloadPath -> PodcastId -> EpisodeId -> Episode -> IO FilePath
downloadEpisode base pid epid ep =
  let url = unpack $ episodeUrl ep
      path = unDownloadPath base </> unpack (unPodcastId pid) </> unpack (unEpisodeId epid) <> takeExtension url
   in const path <$< writeFileLBS path . (^. R.responseBody) =<< R.get url
