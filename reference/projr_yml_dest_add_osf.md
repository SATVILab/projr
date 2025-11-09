# Add an OSF node as a destination

Add an OSF node (project or component) as a destination to a \_projr.yml
file.

## Usage

``` r
projr_yml_dest_add_osf(
  title,
  content,
  path = NULL,
  structure = NULL,
  overwrite = FALSE,
  public = FALSE,
  category = NULL,
  description = NULL,
  id = NULL,
  id_parent = NULL,
  send_cue = NULL,
  send_strategy = NULL,
  send_inspect = NULL,
  profile = "default"
)

projr_yml_dest_add_osf_proj(
  title,
  description = NULL,
  content = NULL,
  public = FALSE,
  id = NULL
)

projr_yml_dest_add_osf_comp(
  title,
  description = NULL,
  content = NULL,
  public = FALSE,
  category = NULL,
  id_parent = NULL,
  id = NULL
)
```

## Arguments

- title:

  character. Title of the OSF node. For GitHub releases, can use title
  as `@version`, Note that this implies that a new tag will be created
  with each new version, so do not use with large files. If not
  supplied, then will automatically be generated from `content`.

- content:

  character vector. Labels of directories to include in the upload.
  Options are the labels of directories in the active `projr`
  configuration, as well as "docs", "data" and "code". "docs" means the
  directory where the documents are rendered to, "data" means the files
  in the `"data"` directory, and "code" means all files tracked by the
  Git repository.

- path:

  character. Path to the directory on the OSF node.

- structure:

  "latest" or "version". Structure of the remote. If "latest", then
  `path` simply contains the latest versions of the contents. If
  "version", then `path` will contain a directory for each version. If
  not supplied, will be `version`.

- overwrite:

  logical. Whether to rewrite an existing entry of the same title in the
  specified `projr` configuration file. Default is TRUE.

- public:

  logical. Whether the OSF node is public. Default is `FALSE`.

- category:

  character. The category of the project or component. The following are
  valid options: `"project"`, `"analysis"`, `"communication"`, `"data"`,
  `"hypothesis"`, `"instrumentation"`, `"methods and measures"`,
  `"procedure"`, `"project"`, `"software"` and other `"other"`. Default
  is `NULL.`

- description:

  character. Description of the OSF node. Default is `NULL`.

- id:

  character. The id of the project or component. Must be five
  characters. Default is `NULL`.

- id_parent:

  character. The id of the parent project or component. Must be five
  characters. Default is `NULL`.

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
