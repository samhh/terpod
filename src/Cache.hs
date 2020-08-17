module Cache (CachedFeed, toCached, getCache, setCache) where

import Data.Functor.Custom ((<$<))
import qualified Data.Map as M
import Data.String.Custom (surround)
import Data.Timestamp (Timestamp, now, timestampCodec)
import Podcast (Episode (..), EpisodeId, FeedId)
import System.Environment.XDG.BaseDir (getUserCacheFile)
import Text.Feed.Query (feedItems, getItemLink, getItemTitle)
import Text.Feed.Query.Custom (getItemId')
import Text.Feed.Types (Feed)
import Toml (TomlCodec, (.=))
import qualified Toml

type CachedFeed = (FeedId, [(EpisodeId, Episode)])

episodeCodec :: TomlCodec Episode
episodeCodec = Episode <$> Toml.text "title" .= title <*> Toml.text "url" .= episodeUrl

data Cache = Cache
  { timestamp :: Timestamp,
    feeds :: Map FeedId (Map EpisodeId Episode)
  }

cacheCodec :: TomlCodec Cache
cacheCodec =
  Cache
    <$> timestampCodec "timestamp" .= timestamp
    <*> Toml.tableMap Toml._KeyText (Toml.tableMap Toml._KeyText (Toml.table episodeCodec)) "feeds" .= feeds

cachePath :: IO FilePath
cachePath = getUserCacheFile "terpod" "synced.toml"

toCached :: FeedId -> Feed -> CachedFeed
toCached fid feed = (fid, morph `mapMaybe` feedItems feed)
  where
    morph x = build <$> getItemId' x <*> getItemTitle x <*> getItemLink x
    build epid eptitle eplink = (epid, Episode eptitle eplink)

getCache :: IO [CachedFeed]
getCache = fmap (second M.toList <$< M.toList . feeds) <$> Toml.decodeFile cacheCodec =<< cachePath

setCache :: [CachedFeed] -> IO ()
setCache xs = do
  path <- cachePath
  ts <- now
  void <$> Toml.encodeToFile cacheCodec path $ Cache ts (M.fromList <$> M.fromList (escape xs))
  where
    -- The string is cut off at an invalid character such as a colon, hence the
    -- need to surround in quotes
    escape = fmap $ second $ fmap $ first $ surround "\""
