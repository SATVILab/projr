# Add a GitHub release as a destination

Add a GitHub release as a destination to a \_projr.yml file.

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
  profile = "default"
)
```

## Arguments

- title:

  character. Title of the GitHub release. Can use title as `@version`,
  in which case the release will be entitled the version of the project
  at the time. If not supplied, then will automatically be generated
  from `content`.

- content:

  character vector. Labels of directories to include in the upload.
  Options are the labels of directories in the active `projr`
  configuration, as well as "docs", "data" and "code".

- structure:

  "latest" or "archive". Structure of the remote. Default is NULL.

- overwrite:

  logical. Whether to rewrite an existing entry. Default is TRUE.

- send_cue:

  "always", "if-change" or "never". When to send to the remote. Default
  is NULL.

- send_strategy:

  "upload-all", "upload-missing", "sync-purge" or "sync-diff". How to
  synchronise to the remote. Default is NULL.

- send_inspect:

  "manifest" , "file" or "none". What to look at to find files on the
  remote. Default is NULL.

- profile:

  character. Profile to write the settings to. Default is "default".
