module Data.Timestamp (now, timestampCodec, Timestamp (..)) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Toml

newtype Timestamp = Timestamp Natural

now :: IO Timestamp
now = Timestamp <$> round <$> getPOSIXTime

timestampCodec :: Toml.Key -> Toml.TomlCodec Timestamp
timestampCodec = Toml.diwrap . Toml.natural
