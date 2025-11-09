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
  configuration, as well as "docs", "data" and "code". "docs" means the
  directory where the documents are rendered to, "data" means the files
  in the `"data"` directory, and "code" means all files tracked by the
  Git repository.

- path:

  character. Path to the directory. If a relative path is given, then it
  is taken as relative to the project home directory. Must be supplied.

- structure:

  "latest" or "version". Structure of the remote. If "latest", then
  `path` simply contains the latest versions of the contents. If
  "version", then `path` will contain a directory for each version. If
  not supplied, will be `version`.

- overwrite:

  logical. Whether to rewrite an existing entry of the same title in the
  specified `projr` configuration file. Default is TRUE.

- send_cue:

  "always", "if-change" or "never". Only relevant if `structure` is
  `archive` and `send_strategy` is `sync-diff` or `sync-purge`. If
  `always`, then a new remote is created every time, even if there is no
  change from the previous build. For example, if the contents of
  `raw-data` are the same between builds `v0.0.1` and `v0.0.2`, then a
  local remote would have folders `raw-data/v0.0.1` and
  `raw-data/v0.0.2`. If `if-change`, then a new remote is created only
  if there is a change from the previous build. In the example above, a
  local remote would only have the folder `raw-data/v0.0.1`. If `never`,
  then a new remote is never created.

- send_strategy:

  "upload-all", "upload-missing", "sync-purge" and "sync-diff". How to
  synchronise to the remote. If `upload-all`, then all files are
  uploaded. If `upload-missing`, then only missing files are uploaded.
  If `sync-purge`, then all files on the remote are deleted before
  uploading all local files. If `sync-diff`, then files that have
  changed or been added locally are uploaded to the remote, and files
  that have been removed locally are removed from the remote. If not
  set, then "sync-diff" will be used.

- send_inspect:

  "manifest" , "file" or "none". What to look at to find what are the
  files on the remote, and their versions. If `manifest`, then the
  manifest on the remote is used. If `file`, then the files on the
  remote are downloaded and their versions are determined. If `none`,
  then no inspection is done (the remote is typically treated as "empty"
  in that case). If not set, then defaults to `"manifest"`.

- profile:

  character. Profile to write the settings to. If "default", then
  written to `_projr.yml`, otherwise written to `_projr-<profile>.yml`.
  The default is "default".
