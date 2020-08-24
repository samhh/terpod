module CLI (parseOptions, Options (..), Command (..), ListOptions (..), Order (..)) where

import Episode (EpisodeId (EpisodeId))
import qualified Options.Applicative as A
import Podcast (PodcastId (PodcastId))

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
    limit :: Maybe Int,
    offset :: Maybe Int,
    order :: Order
  }
  deriving (Show)

data Command
  = Sync
  | List ListOptions
  | Download EpisodeId
  deriving (Show)

syncParser :: A.Parser Command
syncParser = pure Sync

listParser :: A.Parser Command
listParser =
  fmap List $
    ListOptions
      <$> (fmap PodcastId <$> A.optional (A.argument A.str (A.metavar "PODCAST-ID")))
      <*> A.optional (A.option A.auto (A.long "limit" <> A.short 'n' <> A.metavar "LIMIT" <> A.help "Limit the number of items output"))
      <*> A.optional (A.option A.auto (A.long "offset" <> A.metavar "OFFSET" <> A.help "Offset items (prior to limiting)"))
      <*> A.flag Newest Oldest (A.long "oldest" <> A.help "Sort by oldest")

downloadParser :: A.Parser Command
downloadParser = Download . EpisodeId <$> A.argument A.str (A.metavar "EPISODE-ID")

commandParser :: A.Parser Command
commandParser =
  A.subparser
    ( A.command "sync" (A.info syncParser $ A.progDesc "Sync podcast feeds")
        <> A.command "list" (A.info listParser $ A.progDesc "List the latest episodes of all podcasts")
        <> A.command "download" (A.info downloadParser $ A.progDesc "Download an episode by ID")
    )

parser :: A.ParserInfo Options
parser =
  A.info
    (A.helper <*> (Options <$> commandParser))
    ( A.fullDesc
        <> A.progDesc "Manage podcasts from the command-line."
    )

-- | Parse command-line options. The library we're using for this will handle
-- the possibility of failure for us, which isn't encoded in the type
-- signature.
parseOptions :: IO Options
parseOptions = A.execParser parser
