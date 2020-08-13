module Main (main) where

import Control.Lens ((^.))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (unpack)
import Data.Tuple.Sequence (sequenceT)
import qualified Network.Wreq as R
import System.Environment.XDG.BaseDir (getUserConfigFile)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (ItemGetter, feedItems, getFeedTitle, getItemId, getItemTitle)
import Text.Feed.Types (Feed, Item)
import Toml (TomlCodec, TomlDecodeError, (.=))
import qualified Toml

-- | Compose two functions where the first returns a functor and the second is
-- | to be applied within said functor.
(<$<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<$<) = fmap . fmap

infixr 8 <$<

withNewline :: IsString a => Semigroup a => a -> a -> a
withNewline a b = a <> "\n" <> b

data Source = Source
  { url :: !Text
  }

sourceCodec :: TomlCodec Source
sourceCodec = Source <$> Toml.text "url" .= url

data Config = Config
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

main :: IO ()
main =
  getCfg >>= \case
    Left e -> mapM_ (putStrLn . show) e
    Right cfg -> mapM_ render $ M.toList $ sources cfg
  where
    render :: (Text, Source) -> IO ()
    render (id, src) =
      (getPod . unpack . url) src >>= \case
        Nothing -> putStrLn $ "Failed to get feed (id: " <> unpack id <> ")."
        Just feed -> do
          putStrLn $ unpack $ getFeedTitle feed <> " (" <> id <> "):"
          putStrLn $ unpack $ list $ feedItems feed
