# Add a GitHub release as a destination

Register a GitHub release as an archive and select it for production
builds. The release settings are written under `remotes.github.<title>`,
and the selection, with its `content`, under `build.dest`.

## Usage

``` r
projr_yml_dest_add_github(
  title,
  content,
  structure = NULL,
  overwrite = TRUE,
  send_cue = NULL,
  send_strategy = NULL,
  send_inspect = NULL,
  profile = "default",
  branch = NULL
)
```

## Arguments

- title:

  character. Name of the archive under `remotes.github`, also used as
  the GitHub release tag and title. It can contain `@branch`,
  `@version`, and `@uuid`.

- content:

  character vector. Labels of directories to include in the upload.
  Options are the labels of directories in the active `projr`
  configuration, or `code` for the source archive GitHub provides with a
  release tag.

- structure:

  "latest" or "archive". Structure of the remote. Default is NULL.

- overwrite:

  logical. Whether to rewrite an existing entry. Default is TRUE.

- send_cue:

  "always" or "if-change". When to send to the remote. The default sends
  changed content.

- send_strategy:

  "upload-all", "upload-missing", "sync-purge" or "sync-diff". How to
  synchronise to the remote. Default is NULL.

- send_inspect:

  "manifest" , "file" or "none". What to look at to find files on the
  remote. Default is NULL.

- profile:

  character. Profile to write the settings to. Default is "default".

- branch:

  character or NULL. GitHub branch whose current commit a new release
  should target. Use `@branch` for the current checkout branch. The
  default uses GitHub's release target.
