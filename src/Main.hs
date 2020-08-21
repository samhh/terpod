module Main (main) where

import CLI (Command (..), Options (..), parseOptions)
import Cache (CachedPodcast, getCache, setCache, toCached)
import Config (Source (sourceUrl), downloadPath, getCfg, sources)
import Data.List.Extra (firstJust)
import Data.Map ()
import qualified Data.Map as M
import Data.Text (unpack)
import Episode (Episode (title), EpisodeId, downloadEpisode, unEpisodeId)
import Podcast (PodcastId, getPodcast, unPodcastId)
import Text.Feed.Types (Feed)

main :: IO ()
main =
  parseOptions <&> command >>= \case
    Download epid ->
      getCfg >>= \case
        Left e -> mapM_ print e
        Right cfg ->
          getCache <&> firstJust (findEpisode epid) >>= \case
            Nothing -> putStrLn $ "Failed to find synced episode ID: " <> unpack (unEpisodeId epid)
            Just (pid, ep) -> do
              putStrLn $ "Downloading episode: " <> unpack (title ep)
              path <- downloadEpisode (downloadPath cfg) pid epid ep
              putStrLn $ "Finished download, file at: " <> path
    List -> mapM_ renderFeed =<< getCache
    Sync ->
      getCfg >>= \case
        Left e -> mapM_ print e
        Right cfg -> do
          feeds <- catMaybes <$> mapM getPodcasts (M.toList $ sources cfg)
          setCache $ uncurry toCached <$> feeds
          putStrLn "Successfully synced."
  where
    findEpisode :: EpisodeId -> CachedPodcast -> Maybe (PodcastId, Episode)
    findEpisode epid (podid, eps) = fmap (first (const podid)) . find ((== epid) . fst) $ eps

    renderFeed :: CachedPodcast -> IO ()
    renderFeed (fid, eps) = do
      putStrLn $ unpack $ unPodcastId fid
      mapM_ (putStrLn . unpack . renderEpisode) $ take 10 eps

    renderEpisode :: (EpisodeId, Episode) -> Text
    renderEpisode (epid, ep) = "\t" <> unEpisodeId epid <> ": " <> title ep

    getPodcasts :: (PodcastId, Source) -> IO (Maybe (PodcastId, Feed))
    getPodcasts (fid, src) =
      (getPodcast . unpack . sourceUrl) src >>= \case
        Nothing -> Nothing <$ putStrLn ("Failed to get feed (id: " <> unpack (unPodcastId fid) <> ").")
        Just feed -> pure $ Just (fid, feed)
