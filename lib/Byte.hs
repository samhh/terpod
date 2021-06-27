module Byte (Bytes (Bytes), Megabytes (Megabytes), megabytes, friendlySize) where

import           Control.Newtype.Generics (Newtype, unpack)
import qualified Data.Text                as T
import           Numeric                  (showFFloat)

newtype Bytes = Bytes Integer
  deriving (Generic)

instance Newtype Bytes

newtype Megabytes = Megabytes Float
  deriving (Generic)

instance Newtype Megabytes

megabytes :: Bytes -> Megabytes
megabytes = Megabytes . (/ 1000000) . fromInteger . unpack

friendlySize :: Bytes -> Text
friendlySize = (<> "MB") . T.pack . fixedFloat . unpack . megabytes
  where fixedFloat :: Float -> String
        fixedFloat = flip (showFFloat (Just 2)) ""

