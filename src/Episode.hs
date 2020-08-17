module Episode (EpisodeId, Episode (..)) where

type EpisodeId = Text

data Episode = Episode
  { title :: Text,
    episodeUrl :: Text
  }
