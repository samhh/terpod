module Terpod.Config (Config (..), unDownloadPath, DownloadPath (DownloadPath), Source (..), getCfg, expandTilde) where

import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.Directory (getHomeDirectory)
import Terpod.Podcast (PodcastId, _KeyPodcastId)
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

expandTilde :: FilePath -> IO FilePath
expandTilde ('~' : xs) = (<> xs) <$> getHomeDirectory
expandTilde xs = pure xs
