module Episode (EpisodeId (EpisodeId), Episode (..), unEpisodeId, _KeyEpisodeId, downloadEpisode) where

import Control.Lens ((^.))
import Data.Functor.Custom ((<$<))
import Data.Text (pack, unpack)
import qualified Network.Wreq as R
import System.Environment.XDG.BaseDir (getUserDataFile)
import Toml (Key, TomlBiMap)
import Toml.Codec.BiMap.Conversion.Custom (textBiMap)

newtype EpisodeId = EpisodeId Text
  deriving (Show, Eq, Ord)

unEpisodeId :: EpisodeId -> Text
unEpisodeId (EpisodeId x) = x

instance IsString EpisodeId where
  fromString = EpisodeId . pack

instance Semigroup EpisodeId where
  a <> b = a <> b

_KeyEpisodeId :: TomlBiMap Key EpisodeId
_KeyEpisodeId = textBiMap EpisodeId unEpisodeId

data Episode = Episode
  { title :: Text,
    episodeUrl :: Text
  }


downloadPath :: FilePath -> IO FilePath
downloadPath = getUserDataFile "terpod"

-- | Download a podcast episode onto the filesystem.
downloadEpisode :: Episode -> IO FilePath
downloadEpisode ep = do
  path <- downloadPath $ unpack $ title ep
  writeFileLBS path <=< (^. R.responseBody) <$< R.get . unpack . episodeUrl $ ep
  pure path
