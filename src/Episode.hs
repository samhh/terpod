module Episode (episodeIdCodec, EpisodeId (EpisodeId), Episode (..), unEpisodeId, _KeyEpisodeId, downloadEpisode) where

import Config (DownloadPath, expandTilde, unDownloadPath)
import Control.Lens ((^.))
import Data.Text (pack, unpack)
import Data.Time.Calendar (Day)
import qualified Network.Wreq as R
import System.Directory (createDirectoryIfMissing)
import Podcast (PodcastId, unPodcastId)
import System.FilePath.Posix (takeExtension, (</>))
import Toml (TomlBiMap, TomlCodec)
import qualified Toml
import Toml.Codec.BiMap.Conversion.Custom (textBiMap)

newtype EpisodeId = EpisodeId Text
  deriving (Show, Eq, Ord)

unEpisodeId :: EpisodeId -> Text
unEpisodeId (EpisodeId x) = x

instance IsString EpisodeId where
  fromString = EpisodeId . pack

instance Semigroup EpisodeId where
  x <> y = EpisodeId $ unEpisodeId x <> unEpisodeId y

episodeIdCodec :: Toml.Key -> TomlCodec EpisodeId
episodeIdCodec = Toml.diwrap . Toml.text

_KeyEpisodeId :: TomlBiMap Toml.Key EpisodeId
_KeyEpisodeId = textBiMap EpisodeId unEpisodeId

data Episode = Episode
  { episodeId :: EpisodeId,
    title :: Text,
    episodeUrl :: Text,
    publishDate :: Day
  }
  deriving (Show)

instance Eq Episode where
  (==) = (==) `on` episodeId

instance Ord Episode where
  compare = compare `on` publishDate

-- | Download a podcast episode onto the filesystem.
downloadEpisode :: DownloadPath -> PodcastId -> EpisodeId -> Episode -> IO FilePath
downloadEpisode base pid epid ep = do
  let url = unpack $ episodeUrl ep
  dir <- (</> unpack (unPodcastId pid)) <$> expandTilde (unDownloadPath base)
  let path = dir </> unpack (unEpisodeId epid) <> takeExtension url
  createDirectoryIfMissing True dir
  writeFileLBS path . (^. R.responseBody) =<< R.get url
  pure path
