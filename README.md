# terpod

A **ter**minal **pod**cast manager. Check out the issues for a sense of the project's short-term goals.

**Nota bene:** This is very early on in development and instability should be expected. Be happy if it doesn't nuke your machine. (But please report a bug if it does!)

```
Usage: terpod COMMAND
  Manage podcasts from the command-line.

Available options:
  -h,--help                Show this help text

Available commands:
  sync                     Sync podcast feeds.
  list                     List the latest episodes of all podcasts.
  download                 Download an episode by ID.
```

## Usage

Define a list of feeds at `$XDG_CONFIG_HOME/terpod/config.toml`, for example:

```toml
[sources]
bbc-global-news = { url = "https://podcasts.files.bbci.co.uk/p02nq0gn.rss" }
talking-politics = { url = "https://rss.acast.com/talkingpolitics" }
```

Then use the application per the above help text. Downloaded episodes will be stored in `$XDG_DATA_HOME/terpod/`.

## Contributing

At the moment, I'm focused on just getting this to work at all. I've started too many side projects, gotten bogged down in (my idea of) perfection, and as a result never gotten anything done. To that end, there's a lot about the state of the codebase right now that I recognise is suboptimal.

That said, I'm new to Haskell development, and have only minimal experience in writing command-line applications. If you spot anything that's bad or otherwise improvable, please do report it to me or put up a pull request. If nothing else it's good to have this stuff documented somewhere other than my head, and if you've any experience with Haskell you'll certainly spot something I've missed.

Cheers!

