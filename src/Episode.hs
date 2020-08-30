module Episode (episodeIdCodec, EpisodeId (EpisodeId), Episode (..), downloadEpisode) where

import Config (DownloadPath, expandTilde, unDownloadPath)
import Control.Lens ((^.))
import Control.Newtype.Generics (Newtype, unpack)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import qualified Network.Wreq as R
import System.Directory (createDirectoryIfMissing)
import Podcast (PodcastId)
import System.FilePath.Posix (takeExtension, (</>))
import Toml (TomlCodec)
import qualified Toml

newtype EpisodeId = EpisodeId Text
  deriving (Show, Eq, Ord, IsString, Generic)

instance Newtype EpisodeId

episodeIdCodec :: Toml.Key -> TomlCodec EpisodeId
episodeIdCodec = Toml.diwrap . Toml.text

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
downloadEpisode :: DownloadPath -> PodcastId -> Episode -> IO FilePath
downloadEpisode base pid ep = do
  let url = T.unpack $ episodeUrl ep
  dir <- (</> T.unpack (unpack pid)) <$> expandTilde (unDownloadPath base)
  let path = dir </> T.unpack (unpack $ episodeId ep) <> takeExtension url
  createDirectoryIfMissing True dir
  writeFileLBS path . (^. R.responseBody) =<< R.get url
  pure path
