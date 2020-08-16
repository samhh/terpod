module Podcast (FeedId, EpisodeId, Episode (..)) where

type FeedId = Text

type EpisodeId = Text

data Episode = Episode
  { title :: Text,
    episodeUrl :: Text
  }
