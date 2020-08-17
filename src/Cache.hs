module Cache (CachedFeed, toCached, getCache, setCache) where

import Data.Functor.Custom ((<$<))
import qualified Data.Map as M
import Data.String.Custom (surround)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Timestamp (Timestamp, now, timestampCodec)
import Episode (Episode (..), EpisodeId)
import Podcast (PodcastId)
import System.Environment.XDG.BaseDir (getUserCacheFile)
import Text.Feed.Query (feedItems, getItemTitle)
import Text.Feed.Query.Custom (getItemEnclosureLink, getItemId')
import Text.Feed.Types (Feed)
import Toml (TomlCodec, (.=))
import qualified Toml

type CachedFeed = (PodcastId, [(EpisodeId, Episode)])

episodeCodec :: TomlCodec Episode
episodeCodec = Episode <$> Toml.text "title" .= title <*> Toml.text "url" .= episodeUrl

data Cache = Cache
  { timestamp :: Timestamp,
    feeds :: Map PodcastId (Map EpisodeId Episode)
  }

cacheCodec :: TomlCodec Cache
cacheCodec =
  Cache
    <$> timestampCodec "timestamp" .= timestamp
    <*> Toml.tableMap Toml._KeyText (Toml.tableMap Toml._KeyText (Toml.table episodeCodec)) "feeds" .= feeds

cachePath :: IO FilePath
cachePath = getUserCacheFile "terpod" "synced.toml"

toCached :: PodcastId -> Feed -> CachedFeed
toCached fid feed = (fid, morph `mapMaybe` feedItems feed)
  where
    morph x = build <$> getItemId' x <*> getItemTitle x <*> getItemEnclosureLink x
    build epid eptitle eplink = (epid, Episode eptitle eplink)

getCache :: IO [CachedFeed]
getCache = fmap (second M.toList <$< M.toList . feeds) <$> Toml.decodeFile cacheCodec =<< cachePath

setCache :: [CachedFeed] -> IO ()
setCache xs = do
  path <- cachePath
  ts <- now
  let encoded = Toml.encode cacheCodec $ Cache ts (M.fromList <$> M.fromList (escape xs))
  TIO.writeFile path $ unicodePatch encoded
  where
    -- The string is cut off at an invalid character such as a colon, hence the
    -- need to surround in quotes
    escape = fmap $ second $ fmap $ first $ surround "\""
    -- Unicode characters seemingly incorrectly encoded by lib, see:
    -- https://github.com/kowainik/tomland/issues/334
    unicodePatch = T.replace "\\" "\\u0"
