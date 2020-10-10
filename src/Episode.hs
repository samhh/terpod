module Episode (episodeIdCodec, EpisodeId (EpisodeId), Episode (..), downloadEpisode) where

import Byte (friendlySize)
import Conduit (sinkFile, (.|), runConduitRes)
import Config (DownloadPath, expandTilde, unDownloadPath)
import Control.Newtype.Generics (Newtype, unpack)
import Data.Char (isAlphaNum, toLower)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Network (getContentLength)
import Network.HTTP.Simple (httpSource, getResponseBody, parseRequest, Response)
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

-- Although the episode ID isn't very helpful for anything user-facing, it is
-- helpful internally in the codebase as a unique ID (within the same podcast,
-- anyway... hopefully!)
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

sanitise :: String -> String
sanitise = fmap toSafe
  where
    toSafe :: Char -> Char
    toSafe x = if isAlphaNum x then toLower x else '-'

-- | Download a podcast episode onto the filesystem.
downloadEpisode :: DownloadPath -> PodcastId -> Episode -> IO FilePath
downloadEpisode base pid ep = do
  let url = T.unpack $ episodeUrl ep
  req <- parseRequest url
  dir <- (</> T.unpack (unpack pid)) <$> expandTilde (unDownloadPath base)
  let path = dir </> (sanitise . T.unpack . title $ ep) <> takeExtension url
  createDirectoryIfMissing True dir
  runConduitRes $ httpSource req logSizeAndGetBody .| sinkFile path
  pure path
    where logSizeAndGetBody :: MonadIO m => Response (m b) -> m b
          logSizeAndGetBody res = do
            putStrLn . maybe "Unknown length" (("Size: " <>) . friendlySize) . getContentLength $ res
            getResponseBody res
