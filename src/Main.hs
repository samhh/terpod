module Main (main) where

import Cache (setCache, toCached)
import Config (Source (sourceUrl), getCfg, sources)
import Control.Lens ((^.))
import Data.Functor.Custom ((<$<))
import Data.Map ()
import qualified Data.Map as M
import Data.String.Custom (withNewline)
import Data.Text (unpack)
import Data.Tuple.Sequence (sequenceT)
import qualified Network.Wreq as R
import Podcast (FeedId)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (feedItems, getFeedTitle, getItemTitle)
import Text.Feed.Query.Custom (getItemId')
import Text.Feed.Types (Feed, Item)

getPod :: String -> IO (Maybe Feed)
getPod = parseFeedSource . (^. R.responseBody) <$< R.get

list :: [Item] -> Text
list [] = "No items to display."
list xs = foldr withNewline mempty $ mapMaybe item $ take 10 xs
  where
    item = fmt <$< sequenceT . (getItemId' &&& getItemTitle)
    fmt (epid, eptitle) = "\t" <> epid <> ": " <> eptitle

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
