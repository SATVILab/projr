# Add a local directory as a destination

Add a local directory as a destination to a \_projr.yml file.

## Usage

``` r
projr_yml_dest_add_local(
  title,
  content,
  path,
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

  character. The name of the directory. Has no effect besides helping
  structure `_projr.yml`. If not supplied, will be made equal to
  `content`.

- content:

  character vector. Labels of directories to include in the upload.
  Options are the labels of directories in the active `projr`
  configuration.

- path:

  character. Path to the directory. If a relative path is given, then it
  is taken as relative to the project home directory. Must be supplied.

- structure:

  "latest" or "archive". Structure of the remote. If "latest", then
  `path` simply contains the latest versions of the contents. If
  "version", then `path` will contain a directory for each version. If
  not supplied, will be `archive`.

- overwrite:

  logical. Whether to rewrite an existing entry of the same title in the
  specified `projr` configuration file. Default is TRUE.

- send_cue:

  "always", "if-change" or "never". When to send to the remote. Default
  is NULL.

- send_strategy:

  "upload-all", "upload-missing", "sync-purge" or "sync-diff". How to
  synchronise to the remote. Default is NULL.

- send_inspect:

  "manifest" , "file" or "none". What to look at to find what are the
  files on the remote, and their versions. Default is NULL.

- profile:

  character. Profile to write the settings to. If "default", then
  written to `_projr.yml`, otherwise written to `_projr-<profile>.yml`.
  The default is "default".
