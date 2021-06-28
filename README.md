# terpod

A **ter**minal **pod**cast manager.

```
Usage: terpod [-v|--version] COMMAND
  Manage podcasts from the command-line.

Available options:
  -h,--help                Show this help text
  -v,--version             Output version

Available commands:
  sync                     Sync podcast feeds
  list                     List the latest episodes of all podcasts
  download                 Download an episode by ID
```

## Usage

Define a list of feeds at `$XDG_CONFIG_HOME/terpod/config.toml`, for example:

```toml
download-path = "/home/user/podcasts/"

[sources]
bbc-global-news = { url = "https://podcasts.files.bbci.co.uk/p02nq0gn.rss" }
talking-politics = { url = "https://rss.acast.com/talkingpolitics" }
```

You'll first need to sync. Following a sync, any of the synced episode IDs can be downloaded on demand.

## Contributing

terpod is currently built against GHC 8.10.4.

