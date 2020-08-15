module Main (main) where

import Control.Lens ((^.))
import Data.Functor.Custom ((<$<))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (unpack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Tuple.Sequence (sequenceT)
import qualified Network.Wreq as R
import System.Environment.XDG.BaseDir (getUserCacheFile, getUserConfigFile)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (ItemGetter, feedItems, getFeedTitle, getItemId, getItemLink, getItemTitle)
import Text.Feed.Types (Feed, Item)
import Toml (TomlCodec, TomlDecodeError, (.=))
import qualified Toml

withNewline :: IsString a => Semigroup a => a -> a -> a
withNewline a b = a <> "\n" <> b

surround :: Text -> Text -> Text
surround outside inside = outside <> inside <> outside

now :: IO Int
now = round <$> getPOSIXTime

data Episode = Episode
  { title :: Text,
    episodeUrl :: Text
  }

episodeCodec :: TomlCodec Episode
episodeCodec = Episode <$> Toml.text "title" .= title <*> Toml.text "url" .= episodeUrl

type FeedId = Text

type EpisodeId = Text

type CachedFeed = (FeedId, [(EpisodeId, Episode)])

data Cache = Cache
  { timestamp :: Int,
    feeds :: Map FeedId (Map EpisodeId Episode)
  }

cacheCodec :: TomlCodec Cache
cacheCodec =
  Cache
    <$> Toml.int "timestamp" .= timestamp
    <*> Toml.tableMap Toml._KeyText (Toml.tableMap Toml._KeyText (Toml.table episodeCodec)) "feeds" .= feeds

cachePath :: IO FilePath
cachePath = getUserCacheFile "terpod" "synced.toml"

setCache :: [CachedFeed] -> IO ()
setCache xs = do
  path <- cachePath
  ts <- now
  Toml.encodeToFile cacheCodec path $ Cache ts (M.fromList <$> M.fromList (escape xs))
  pure ()
  where
    -- The string is cut off at an invalid character such as a colon, hence the
    -- need to surround in quotes
    escape = fmap $ second $ fmap $ first $ surround "\""

newtype Source = Source
  { sourceUrl :: Text
  }

sourceCodec :: TomlCodec Source
sourceCodec = Source <$> Toml.text "url" .= sourceUrl

newtype Config = Config
  { sources :: Map Text Source
  }

cfgCodec :: TomlCodec Config
cfgCodec = Config <$> Toml.tableMap Toml._KeyText (Toml.table sourceCodec) "sources" .= sources

cfgPath :: IO FilePath
cfgPath = getUserConfigFile "terpod" "config.toml"

getCfg :: IO (Either [TomlDecodeError] Config)
getCfg = Toml.decodeFileEither cfgCodec =<< cfgPath

getPod :: String -> IO (Maybe Feed)
getPod = parseFeedSource . (^. R.responseBody) <$< R.get

getItemId' :: ItemGetter Text
getItemId' = snd <$< getItemId

list :: [Item] -> Text
list [] = "No items to display."
list xs = foldr withNewline mempty $ mapMaybe item $ take 10 xs
  where
    item = fmt <$< sequenceT . (getItemId' &&& getItemTitle)
    fmt (id, title) = "\t" <> id <> ": " <> title

toCached :: FeedId -> Feed -> CachedFeed
toCached fid feed = (fid, morph `mapMaybe` feedItems feed)
  where
    morph x = build <$> getItemId' x <*> getItemTitle x <*> getItemLink x
    build epid title link = (epid, Episode title link)

main :: IO ()
main =
  getCfg >>= \case
    Left e -> mapM_ print e
    Right cfg -> do
      x <- fmap catMaybes <$> mapM render $ M.toList $ sources cfg
      setCache $ uncurry toCached <$> x
  where
    render :: (FeedId, Source) -> IO (Maybe (FeedId, Feed))
    render (fid, src) =
      (getPod . unpack . sourceUrl) src >>= \case
        Nothing -> do
          putStrLn $ "Failed to get feed (id: " <> unpack fid <> ")."
          pure Nothing
        Just feed -> do
          putStrLn $ unpack $ getFeedTitle feed <> " (" <> fid <> "):"
          putStrLn $ unpack $ list $ feedItems feed
          pure $ Just (fid, feed)
