module Toml.Codec.BiMap.Conversion.Custom (textBiMap) where

import           Toml (BiMap (BiMap), Key, TomlBiMap,
                       TomlBiMapError (ArbitraryError), backward, forward,
                       parseKey, prettyKey, unTomlParseError)

textBiMap :: (Text -> a) -> (a -> Text) -> TomlBiMap Key a
textBiMap to from =
  BiMap
    { forward = Right . to . prettyKey,
      backward = first (ArbitraryError . unTomlParseError) . parseKey . from
    }
