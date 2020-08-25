module Main (main) where

import CLI (Command (..), ListOptions (..), Options (..), Order (..), parseOptions)
import Cache (CachedPodcast, findEpisode, getCache, setCache, toCached)
import Config (Config, Source (sourceUrl), downloadPath, getCfg, sources)
import Data.List.Extra (firstJust)
import Data.Map ()
import qualified Data.Map as M
import Data.Text (unpack)
import Episode (Episode (title), EpisodeId, downloadEpisode, unEpisodeId)
import Podcast (PodcastId, getPodcast, unPodcastId)
import Text.Feed.Types (Feed)
import Toml (TomlDecodeError)

getPodcasts :: (PodcastId, Source) -> IO (Maybe (PodcastId, Feed))
getPodcasts (fid, src) =
  (getPodcast . unpack . sourceUrl) src >>= \case
    Nothing -> Nothing <$ putStrLn ("Failed to get feed (id: " <> unpack (unPodcastId fid) <> ").")
    Just feed -> pure $ Just (fid, feed)

renderEpisode :: (EpisodeId, Episode) -> Text
renderEpisode (epid, ep) = "\t" <> unEpisodeId epid <> ": " <> title ep

renderFeed :: ListOptions -> CachedPodcast -> IO ()
renderFeed ListOptions {order, limit, offset} (fid, eps) = do
  putStrLn $ unpack $ unPodcastId fid
  let xs = take (10 `fromMaybe` limit) $ drop (0 `fromMaybe` offset) $ sortBy (cmp `on` snd) eps
  mapM_ (putStrLn . unpack . renderEpisode) xs
  where
    cmp :: Ord a => a -> a -> Ordering
    cmp = case order of
      Newest -> flip compare
      Oldest -> compare

renderFailedDecode :: Foldable f => f TomlDecodeError -> IO ()
renderFailedDecode = mapM_ print

download :: Config -> EpisodeId -> IO ()
download cfg epid =
  getCache <&> firstJust (findEpisode epid) >>= \case
    Nothing -> putStrLn $ "Failed to find synced episode ID: " <> unpack (unEpisodeId epid)
    Just (pid, ep) -> do
      putStrLn $ "Downloading episode: " <> unpack (title ep)
      path <- downloadEpisode (downloadPath cfg) pid epid ep
      putStrLn $ "Finished download, file at: " <> path

list :: Foldable f => ListOptions -> f CachedPodcast -> IO ()
list opts = case podcastId opts of
  Nothing -> mapM_ $ renderFeed opts
  Just pid ->
    find ((== pid) . fst) >>> \case
      Nothing -> putStrLn $ "Failed to find synced podcast ID: " <> unpack (unPodcastId pid)
      Just pod -> renderFeed opts pod

sync :: Config -> IO ()
sync cfg = do
  feeds <- catMaybes <$> mapM getPodcasts (M.toList $ sources cfg)
  setCache $ uncurry toCached <$> feeds
  putStrLn "Successfully synced."

main :: IO ()
main =
  parseOptions <&> command >>= \case
    Download epid -> either renderFailedDecode (flip download $ epid) =<< getCfg
    List opts -> list opts =<< getCache
    Sync -> either renderFailedDecode sync =<< getCfg
