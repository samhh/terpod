module Main (main) where

import CLI (Command (..), Options (..), parseOptions)
import Cache (CachedFeed, getCache, setCache, toCached)
import Config (Source (sourceUrl), getCfg, sources)
import Data.Map ()
import qualified Data.Map as M
import Data.Text (unpack)
import Episode (Episode (title), EpisodeId, unEpisodeId)
import Podcast (PodcastId, getPodcast)
import Text.Feed.Types (Feed)

main :: IO ()
main =
  parseOptions <&> command >>= \case
    Download epid -> putStrLn $ "TODO: Download " <> unpack (unEpisodeId epid)
    List -> mapM_ renderFeed =<< getCache
    Sync ->
      getCfg >>= \case
        Left e -> mapM_ print e
        Right cfg -> do
          feeds <- catMaybes <$> mapM getFeeds (M.toList $ sources cfg)
          setCache $ uncurry toCached <$> feeds
          putStrLn "Successfully synced."
  where
    renderFeed :: CachedFeed -> IO ()
    renderFeed (fid, eps) = do
      putStrLn $ unpack fid
      mapM_ (putStrLn . unpack . renderEpisode) $ take 10 eps

    renderEpisode :: (EpisodeId, Episode) -> Text
    renderEpisode (epid, ep) = "\t" <> (unEpisodeId epid) <> ": " <> title ep

    getFeeds :: (PodcastId, Source) -> IO (Maybe (PodcastId, Feed))
    getFeeds (fid, src) =
      (getPodcast . unpack . sourceUrl) src >>= \case
        Nothing -> Nothing <$ putStrLn ("Failed to get feed (id: " <> unpack fid <> ").")
        Just feed -> pure $ Just (fid, feed)
