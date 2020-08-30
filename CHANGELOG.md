# Changelog

## 0.2.0 -- 2020-08-30

This release smooths out a lot of rough edges from the initial release, and adds some list manipulation flags:

- The list command now allows an optional podcast ID argument
- The list command has additionally been augmented with three new flags: `-n`/`--limit`, `--offset`, and `--oldest`
- Syncing will now continue to try and sync what it can if some feeds fail to sync
- Feeds are now synced in parallel, speeding things up quite a bit
- Fixed download failing to write if episode ID contained special characters
- There are new help flags (`-h`/`--help`) available for all subcommands
- Tilde (`~`) character, expanding to user home directory, is now supported in the download path config property
- Attempted to fix sync failing when certain characters are encountered in feeds - whilst we wait for a proper fix upstream, some episode titles might look a bit off

Note that the cache's file structure has changed with this release, so a sync will be required before the other commands will work properly.

## 0.1.0 -- 2020-08-23

This is terpod's initial release!

Support for the following is included with this release:

- Configure feeds via a TOML config file
- Sync feeds on demand
- List the latest synced episodes of all feeds
- Download an episode by synced ID

