module Cache (CachedPodcast, toCached, getCache, setCache) where

import Data.Functor.Custom ((<$<))
import qualified Data.Map as M
import Data.String.Custom (surround, unsurround)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Timestamp (Timestamp, now, timestampCodec)
import Episode (Episode (..), EpisodeId (EpisodeId), episodeIdCodec, unEpisodeId, _KeyEpisodeId)
import Podcast (PodcastId, _KeyPodcastId)
import System.Environment.XDG.BaseDir (getUserCacheFile)
import Text.Feed.Query (feedItems, getItemPublishDate, getItemTitle)
import Text.Feed.Query.Custom (getItemEnclosureLink, getItemId')
import Text.Feed.Types (Feed)
import Toml (TomlCodec, (.=))
import qualified Toml

type CachedPodcast = (PodcastId, [(EpisodeId, Episode)])

episodeCodec :: TomlCodec Episode
episodeCodec =
  Episode
    <$> episodeIdCodec "episode-id" .= episodeId
    <*> Toml.text "title" .= title
    <*> Toml.text "url" .= episodeUrl
    <*> Toml.day "publish-date" .= publishDate

data Cache = Cache
  { timestamp :: Timestamp,
    feeds :: Map PodcastId (Map EpisodeId Episode)
  }
  deriving (Show)

cacheCodec :: TomlCodec Cache
cacheCodec =
  Cache
    <$> timestampCodec "timestamp" .= timestamp
    <*> Toml.tableMap _KeyPodcastId (Toml.tableMap _KeyEpisodeId (Toml.table episodeCodec)) "feeds" .= feeds

cachePath :: IO FilePath
cachePath = getUserCacheFile "terpod" "synced.toml"

toCached :: PodcastId -> Feed -> CachedPodcast
toCached fid feed = (fid, morph `mapMaybe` feedItems feed)
  where
    morph x = build <$> getItemId' x <*> getItemTitle x <*> getItemEnclosureLink x <*> join (getItemPublishDate x)
    build rawEpId epTitle epLink epDate =
      let epId = EpisodeId rawEpId
       in (epId, Episode epId epTitle epLink epDate)

getCache :: IO [CachedPodcast]
getCache = fmap (unescape <$> second M.toList <$< M.toList . feeds) . Toml.decodeFile cacheCodec =<< cachePath
  where
    -- Reverse escaping from setting cache
    unescape :: [CachedPodcast] -> [CachedPodcast]
    unescape = fmap $ second $ fmap $ first (EpisodeId . unsurround "\"" . unEpisodeId)

setCache :: [CachedPodcast] -> IO ()
setCache xs = do
  path <- cachePath
  ts <- now
  let encoded = Toml.encode cacheCodec $ Cache ts $ M.fromList <$> M.fromList (fmap escape xs)
  TIO.writeFile path $ unicodePatch encoded
  where
    -- The string is cut off at an invalid character such as a colon, hence the
    -- need to surround in quotes
    escape :: CachedPodcast -> CachedPodcast
    escape = second $ fmap $ first $ surround "\""

    -- Unicode characters seemingly incorrectly encoded by lib, see:
    -- https://github.com/kowainik/tomland/issues/334
    unicodePatch :: Text -> Text
    unicodePatch = T.replace "\\" "\\u0"
