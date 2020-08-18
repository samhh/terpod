module Config (Config (..), Source (..), getCfg) where

import Podcast (PodcastId, _KeyPodcastId)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import Toml (TomlCodec, TomlDecodeError, (.=))
import qualified Toml

newtype Source = Source
  { sourceUrl :: Text
  }

sourceCodec :: TomlCodec Source
sourceCodec = Source <$> Toml.text "url" .= sourceUrl

newtype Config = Config
  { sources :: Map PodcastId Source
  }

cfgCodec :: TomlCodec Config
cfgCodec = Config <$> Toml.tableMap _KeyPodcastId (Toml.table sourceCodec) "sources" .= sources

cfgPath :: IO FilePath
cfgPath = getUserConfigFile "terpod" "config.toml"

getCfg :: IO (Either [TomlDecodeError] Config)
getCfg = Toml.decodeFileEither cfgCodec =<< cfgPath
