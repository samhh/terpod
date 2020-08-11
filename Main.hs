module Main (main) where

import Control.Lens ((^.))
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

data Config = Config
  { url :: !Text
  }

cfgCodec :: TomlCodec Config
cfgCodec = Config <$> Toml.text "url" .= url

cfgPath :: IO FilePath
cfgPath = getUserConfigFile "terpod" "config.toml"

cfg :: IO (Either [TomlDecodeError] Config)
cfg = Toml.decodeFileEither cfgCodec =<< cfgPath

getPod :: String -> IO (Maybe Feed)
getPod = parseFeedSource . (^. R.responseBody) <$< R.get

getItemId' :: ItemGetter Text
getItemId' = snd <$< getItemId

list :: [Item] -> Text
list [] = "No items to display."
list xs = foldr withNewline mempty $ mapMaybe item xs
  where
    item = fmt <$< sequenceT . (getItemId' &&& getItemTitle)
    fmt (id, title) = id <> ": " <> title

main :: IO ()
main =
  cfg <&> (unpack . fromRight "FAIL" . fmap url) >>= getPod >>= \case
    Nothing -> putStrLn "Failed to parse feed"
    Just feed -> do
      putStrLn $ unpack $ getFeedTitle feed
      putStrLn $ unpack $ list $ feedItems feed
