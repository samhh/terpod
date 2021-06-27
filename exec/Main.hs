module Main (main) where

import           Byte                     (Bytes, friendlySize)
import           CLI                      (Command (..), ListOptions (..),
                                           Options (..), Order (..),
                                           parseOptions)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Newtype.Generics (unpack)
import           Data.List.Extra          (firstJust)
import           Data.Map                 ()
import qualified Data.Map                 as M
import qualified Data.Text                as T
import           Data.Tuple.Custom        (dup)
import           System.Console.ANSI      (clearLine, cursorUpLine)
import           Terpod.Cache             (CachedPodcast, findEpisode, getCache,
                                           setCache, toCached)
import           Terpod.Config            (Config, Source (sourceUrl),
                                           downloadPath, getCfg, sources)
import           Terpod.Episode           (Episode (title), downloadEpisode)
import           Terpod.Podcast           (PodcastId, getPodcast)
import           Text.Feed.Types          (Feed)
import           Toml                     (TomlDecodeError)

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
      putTextLn $ "Downloading episode: " <> title ep <> "\n"
      path <- downloadEpisode renderProgress (downloadPath cfg) pid ep
      putStrLn $ "Finished download, file at: " <> path

  where renderProgress :: (Bytes, Maybe Bytes) -> IO ()
        renderProgress (dl, mtotal) =
          let total = maybe "?" friendlySize mtotal
              output = "Downloaded: " <> T.pack (friendlySize dl) <> "/" <> T.pack total
           in cursorUpLine 1 *> clearLine *> putTextLn output

list :: Foldable f => ListOptions -> f CachedPodcast -> IO ()
list opts = case podcastId opts of
  Nothing -> mapM_ $ renderFeed opts
  Just pid ->
    find ((== pid) . fst) >>> \case
      Nothing  -> putTextLn $ "Failed to find synced podcast ID: " <> unpack pid
      Just pod -> renderFeed opts pod

newtype Count = Count Int deriving (Num)
newtype Total = Total Int deriving (Num)
newtype Failed = Failed [PodcastId] deriving (Semigroup)

sync :: Config -> IO ()
sync cfg = do
  counter <- newIORef (Count 0)
  failed <- newIORef (Failed [])

  let pods = M.toList . sources $ cfg
  let total = Total $ length pods

  renderStart total
  feeds <- catMaybes <$> mapConcurrently (syncOne (pushFailed failed) (renderSuccess total counter)) pods
  renderFin total counter
  renderFailed =<< readIORef failed

  setCache $ uncurry toCached <$> feeds

  where renderCount :: Text -> Total -> Count -> IO ()
        renderCount t (Total total) (Count count) =
          putTextLn $ t <> ": " <> show count <> "/" <> show total

        renderClearedCount :: Text -> Total -> Count -> IO ()
        renderClearedCount x y z = cursorUpLine 1 *> clearLine *> renderCount x y z

        renderStart :: Total -> IO ()
        renderStart = flip (renderCount "Syncing") (Count 0)

        renderSuccess :: Total -> IORef Count -> IO ()
        renderSuccess total =
          renderClearedCount "Syncing" total <=< flip atomicModifyIORef' (dup . (+ Count 1))

        renderFin :: Total -> IORef Count -> IO ()
        renderFin total = renderClearedCount "Synced" total <=< readIORef

        renderFailed :: Failed -> IO ()
        renderFailed (Failed []) = pure ()
        renderFailed (Failed xs) =
          let y = intercalate ", " (T.unpack . unpack <$> xs)
           in putStrLn $ "The following feeds failed to sync: " <> y

        pushFailed :: IORef Failed -> PodcastId -> IO ()
        pushFailed x = modifyIORef' x . (<>) . Failed . pure

        syncOne :: (PodcastId -> IO ()) -> IO () -> (PodcastId, Source) -> IO (Maybe (PodcastId, Feed))
        syncOne onFailure onSuccess (fid, src) = do
          res <- getPodcast . T.unpack . sourceUrl $ src
          if isRight res then onSuccess else onFailure fid
          pure $ (fid,) <$> fromRight Nothing res

main :: IO ()
main =
  parseOptions <&> command >>= \case
    Download pid i -> either renderFailedDecode (\cfg -> download cfg pid i) =<< getCfg
    List opts -> list opts =<< getCache
    Sync -> either renderFailedDecode sync =<< getCfg
