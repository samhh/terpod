module Terpod.Episode (episodeIdCodec, EpisodeId (EpisodeId), Episode (..), downloadEpisode, sanitise) where

import           Byte                     (Bytes (Bytes))
import           Conduit                  (ConduitT, ResourceT, runResourceT,
                                           sealConduitT, sinkFile, yield,
                                           ($$+-), (.|))
import           Control.Newtype.Generics (Newtype, unpack)
import qualified Data.ByteString          as BS
import           Data.Char                (isAlphaNum, toLower)
import           Data.Conduit             (await)
import qualified Data.Text                as T
import           Data.Time.Calendar       (Day)
import           Network                  (getContentLength)
import           Network.HTTP.Conduit     (http, newManager, responseBody,
                                           tlsManagerSettings)
import           Network.HTTP.Simple      (parseRequest)
import           System.Directory         (createDirectoryIfMissing)
import           System.FilePath.Posix    (takeExtension, (</>))
import           Terpod.Config            (DownloadPath, expandTilde,
                                           unDownloadPath)
import           Terpod.Podcast           (PodcastId)
import           Toml                     (TomlCodec)
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
  { episodeId   :: EpisodeId,
    title       :: Text,
    episodeUrl  :: Text,
    publishDate :: Day
  }
  deriving (Show)

instance Eq Episode where
  (==) = (==) `on` episodeId

instance Ord Episode where
  compare = compare `on` publishDate

sanitise :: Text -> Text
sanitise = T.map toSafe
  where
    toSafe :: Char -> Char
    toSafe x = if isAlphaNum x then toLower x else '-'

-- | Download a podcast episode onto the filesystem.
downloadEpisode :: ((Bytes, Maybe Bytes) -> IO ()) -> DownloadPath -> PodcastId -> Episode -> IO FilePath
downloadEpisode onProgress base pid ep = do
  let url = T.unpack $ episodeUrl ep
  req <- parseRequest url
  dir <- (</> T.unpack (unpack pid)) <$> expandTilde (unDownloadPath base)
  let path = dir </> (T.unpack . sanitise . title $ ep) <> takeExtension url
  createDirectoryIfMissing True dir
  man <- newManager tlsManagerSettings
  runResourceT $ do
    res <- http req man
    let total = getContentLength res
    sealConduitT (responseBody res) $$+- callbackProgress total 0 .| sinkFile path
  pure path
    where callbackProgress :: Maybe Bytes -> Int -> ConduitT ByteString ByteString (ResourceT IO) ()
          callbackProgress total curr = await >>= foldMap inspect
            where inspect data' = do
                    let curr' = curr + BS.length data'
                    liftIO . onProgress . (, total) . Bytes . toInteger $ curr'
                    yield data'
                    callbackProgress total curr'
