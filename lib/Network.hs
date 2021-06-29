module Network (getContentLength, getContentType) where

import           Byte                  (Bytes (Bytes))
import qualified Data.ByteString.Char8 as BS
import           Data.Functor.Custom   ((<$<))
import           Network.HTTP.Simple   (Response, getResponseHeader)

getContentLength :: Response a -> Maybe Bytes
getContentLength = Bytes <$< readMaybe <=< BS.unpack <$< viaNonEmpty head . getResponseHeader "Content-Length"

getContentType :: Response a -> Maybe ByteString
getContentType = viaNonEmpty head . getResponseHeader "Content-Type"

