module Main (main) where

import           CLI                      (Command (..), ListOptions (..),
                                           Options (..), Order (..),
                                           parseOptions)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Newtype.Generics (unpack)
import           Data.List.Extra          (firstJust)
import           Data.Map                 ()
import qualified Data.Map                 as M
import qualified Data.Text                as T
import           Terpod.Cache             (CachedPodcast, findEpisode, getCache,
                                           setCache, toCached)
import           Terpod.Config            (Config, Source (sourceUrl),
                                           downloadPath, getCfg, sources)
import           Terpod.Episode           (Episode (title), downloadEpisode)
import           Terpod.Podcast           (PodcastId, getPodcast)
import           Text.Feed.Types          (Feed)
import           Toml                     (TomlDecodeError)

renderGetPodcast :: (PodcastId, Source) -> IO (Maybe (PodcastId, Feed))
renderGetPodcast (fid, src) = do
  putTextLn $ ">>= Syncing " <> unpack fid <> "..."
  res <- getPodcast . T.unpack . sourceUrl $ src
  putTextLn $ case res of
    Left _ -> "<#> Failed to fetch " <> unpack fid <> "!"
    Right x -> case x of
      Just _  -> ">>> Synced " <> unpack fid <> "."
      Nothing -> "<#> Failed to decode " <> unpack fid <> "!"
  pure $ (fid,) <$> fromRight Nothing res

renderEpisode :: (Int, Episode) -> Text
renderEpisode (i, ep) = "\t" <> show i <> ": " <> title ep

renderFeed :: ListOptions -> CachedPodcast -> IO ()
renderFeed ListOptions {order, limit, offset} (fid, eps) = do
  putTextLn $ unpack fid
  let xs = applyLimit . applyOffset . sortEps . withIndices $ eps
  mapM_ (putTextLn . renderEpisode) xs
  where
    withIndices :: [a] -> [(Int, a)]
    withIndices = zip [0..]

    cmp :: Ord a => a -> a -> Ordering
    cmp = case order of
      Newest -> flip compare
      Oldest -> compare

    sortEps :: Ord b => [(a, b)] -> [(a, b)]
    sortEps = sortBy (cmp `on` snd)

    applyOffset :: [a] -> [a]
    applyOffset = drop (0 `fromMaybe` offset)

    applyLimit :: [a] -> [a]
    applyLimit = take (10 `fromMaybe` limit)

renderFailedDecode :: Foldable f => f TomlDecodeError -> IO ()
renderFailedDecode = mapM_ print

download :: Config -> PodcastId -> Int -> IO ()
download cfg pid i =
  getCache <&> firstJust (findEpisode pid i) >>= \case
    Nothing -> putTextLn $ "Failed to find synced episode at index: " <> show i
    Just ep -> do
      putTextLn $ "Downloading episode: " <> title ep
      path <- downloadEpisode (downloadPath cfg) pid ep
      putStrLn $ "Finished download, file at: " <> path

list :: Foldable f => ListOptions -> f CachedPodcast -> IO ()
list opts = case podcastId opts of
  Nothing -> mapM_ $ renderFeed opts
  Just pid ->
    find ((== pid) . fst) >>> \case
      Nothing  -> putTextLn $ "Failed to find synced podcast ID: " <> unpack pid
      Just pod -> renderFeed opts pod

sync :: Config -> IO ()
sync cfg = do
  feeds <- catMaybes <$> mapConcurrently renderGetPodcast (M.toList $ sources cfg)
  setCache $ uncurry toCached <$> feeds

main :: IO ()
main =
  parseOptions <&> command >>= \case
    Download pid i -> either renderFailedDecode (\cfg -> download cfg pid i) =<< getCfg
    List opts -> list opts =<< getCache
    Sync -> either renderFailedDecode sync =<< getCfg
