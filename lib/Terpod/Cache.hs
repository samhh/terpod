module Terpod.Cache (CachedPodcast, toCached, getCache, setCache, findEpisode, getPodcastIds) where

import           Data.Functor.Custom            ((<$<))
import qualified Data.Map                       as M
import qualified Data.Text.IO                   as TIO
import           Data.Time                      (LocalTime, getZonedTime,
                                                 zonedTimeToLocalTime)
import           System.Directory               (createDirectoryIfMissing)
import           System.Environment.XDG.BaseDir (getUserCacheDir)
import           System.FilePath.Posix          ((</>))
import           Terpod.Episode                 (Episode (..),
                                                 EpisodeId (EpisodeId),
                                                 episodeIdCodec)
import           Terpod.Podcast                 (PodcastId, _KeyPodcastId)
import           Text.Feed.Query                (feedItems, getItemPublishDate,
                                                 getItemTitle)
import           Text.Feed.Query.Custom         (getItemEnclosureLink,
                                                 getItemId')
import           Text.Feed.Types                (Feed)
import           Toml                           (TomlCodec, (.=))
import qualified Toml

type CachedPodcast = (PodcastId, [Episode])

episodeCodec :: TomlCodec Episode
episodeCodec =
  Episode
    <$> episodeIdCodec "episode-id" .= episodeId
    <*> Toml.text "title" .= title
    <*> Toml.text "url" .= episodeUrl
    <*> Toml.day "publish-date" .= publishDate

data Cache = Cache
  { timestamp :: LocalTime,
    feeds     :: Map PodcastId [Episode]
  }
  deriving (Show)

cacheCodec :: TomlCodec Cache
cacheCodec =
  Cache
    <$> Toml.localTime "timestamp" .= timestamp
    <*> Toml.tableMap _KeyPodcastId (Toml.list episodeCodec) "feeds" .= feeds

cacheDir :: IO FilePath
cacheDir = getUserCacheDir "terpod"

withCacheFile :: FilePath -> FilePath
withCacheFile = (</> "synced.toml")

toCached :: PodcastId -> Feed -> CachedPodcast
toCached fid feed = (fid, morph `mapMaybe` feedItems feed)
  where
    morph x = build <$> getItemId' x <*> getItemTitle x <*> getItemEnclosureLink x <*> join (getItemPublishDate x)
    build rawEpId epTitle epLink epDate = Episode (EpisodeId rawEpId) epTitle epLink epDate

getCache :: IO [CachedPodcast]
getCache = M.toList . feeds <$< Toml.decodeFile cacheCodec . withCacheFile =<< cacheDir

setCache :: [CachedPodcast] -> IO ()
setCache xs = do
  dir <- cacheDir
  ts <- zonedTimeToLocalTime <$> getZonedTime
  let encoded = Toml.encode cacheCodec $ Cache ts $ M.fromList xs
  createDirectoryIfMissing True dir
  TIO.writeFile (withCacheFile dir) encoded

getPodcastIds :: IO [PodcastId]
getPodcastIds = fmap fst <$> getCache

findEpisode :: PodcastId -> Int -> CachedPodcast -> Maybe Episode
findEpisode mpid i (pid, eps)
  | mpid == pid = eps !!? i
  | otherwise = Nothing
