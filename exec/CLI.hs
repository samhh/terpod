module CLI (parseOptions, Options (..), Command (..), ListOptions (..), Order (..)) where

import           Control.Newtype.Generics (unpack)
import qualified Data.Text                as T
import           Data.Version             (showVersion)
import qualified Options.Applicative      as A
import           Paths_terpod             (version)
import           Terpod.Cache             (getPodcastIds)
import           Terpod.Podcast           (PodcastId (PodcastId))

newtype Options = Options
  { command :: Command
  }
  deriving (Show)

data Order
  = Newest
  | Oldest
  deriving (Show)

data ListOptions = ListOptions
  { podcastId :: Maybe PodcastId,
    limit     :: Maybe Int,
    offset    :: Maybe Int,
    order     :: Order
  }
  deriving (Show)

data Command
  = Sync
  | List ListOptions
  | Download PodcastId Int
  deriving (Show)

pidCompleter :: A.Completer
pidCompleter = A.mkCompleter f
  where f x = filter (x `isPrefixOf`) . fmap (T.unpack . unpack) <$> getPodcastIds

syncParser :: A.Parser Command
syncParser = pure Sync

listParser :: A.Parser Command
listParser = fmap List $ ListOptions
  <$> (fmap PodcastId <$> A.optional (A.argument A.str (A.metavar "PODCAST-ID" <> A.completer pidCompleter)))
  <*> A.optional (A.option A.auto (A.long "limit" <> A.short 'n' <> A.metavar "LIMIT" <> A.help "Limit the number of items output"))
  <*> A.optional (A.option A.auto (A.long "offset" <> A.metavar "OFFSET" <> A.help "Offset items (prior to limiting)"))
  <*> A.flag Newest Oldest (A.long "oldest" <> A.help "Sort by oldest")

downloadParser :: A.Parser Command
downloadParser = Download
  <$> A.argument A.str (A.metavar "PODCAST-ID" <> A.completer pidCompleter)
  <*> A.argument A.auto (A.metavar "INDEX")

commandParser :: A.Parser Command
commandParser = A.subparser $
     A.command "sync" (A.info (A.helper <*> syncParser) $ A.progDesc "Sync podcast feeds")
  <> A.command "list" (A.info (A.helper <*> listParser) $ A.progDesc "List the latest episodes of all podcasts")
  <> A.command "download" (A.info (A.helper <*> downloadParser) $ A.progDesc "Download an episode by ID")

parser :: A.ParserInfo Options
parser = A.info
  (A.helper <*> v <*> (Options <$> commandParser))
  (A.fullDesc <> A.progDesc d)
  where d = "Manage podcasts from the command-line."
        v = A.infoOption (showVersion version) (A.short 'v' <> A.long "version" <> A.help "Output version" <> A.hidden)

-- | Parse command-line options. The library we're using for this will handle
-- the possibility of failure for us, which isn't encoded in the type
-- signature.
parseOptions :: IO Options
parseOptions = A.execParser parser
