module Config (Config (..), unDownloadPath, DownloadPath (DownloadPath), Source (..), getCfg) where

import Podcast (PodcastId, _KeyPodcastId)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import Toml (TomlCodec, TomlDecodeError, (.=))
import qualified Toml

newtype DownloadPath = DownloadPath FilePath

unDownloadPath :: DownloadPath -> FilePath
unDownloadPath (DownloadPath x) = x

downloadPathCodec :: Toml.Key -> TomlCodec DownloadPath
downloadPathCodec = Toml.diwrap . Toml.string

newtype Source = Source
  { sourceUrl :: Text
  }

sourceCodec :: TomlCodec Source
sourceCodec = Source <$> Toml.text "url" .= sourceUrl

data Config = Config
  { downloadPath :: DownloadPath,
    sources :: Map PodcastId Source
  }

cfgCodec :: TomlCodec Config
cfgCodec =
  Config
    <$> downloadPathCodec "download-path" .= downloadPath
    <*> Toml.tableMap _KeyPodcastId (Toml.table sourceCodec) "sources" .= sources

cfgPath :: IO FilePath
cfgPath = getUserConfigFile "terpod" "config.toml"

getCfg :: IO (Either [TomlDecodeError] Config)
getCfg = Toml.decodeFileEither cfgCodec =<< cfgPath
