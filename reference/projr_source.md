# Fetch raw inputs from their sources

Checks and fetches directories from the remotes that supply them,
outside a build. Without `remote`, `projr_source()` uses the remotes
selected under `build.source`, or else every remote registered under
`remotes`. A label offered by more than one registered remote must be
fetched with `remote`.

Arguments override the configured `get` settings for this call. Files
that still match the last fetch are updated. Files changed locally since
then are conflicts: an interactive session asks whether to keep or
replace them, and otherwise `conflict` decides. Nothing changes until
every selected label has been checked.

## Usage

``` r
projr_source(
  label = NULL,
  remote = NULL,
  version = NULL,
  cue = NULL,
  strategy = NULL,
  inspect = NULL,
  conflict = NULL
)
```

## Arguments

- label:

  Character. Directory labels to fetch. Defaults to each source's
  selection, which is every declared raw directory unless `build.source`
  lists content.

- remote:

  Character. A remote registered under `remotes`, named by its title or
  as `<type>:<title>` when the title is used by both types.

- version:

  Character. `"latest"`, a major version (`"1"`), a minor version
  (`"1.2"`), an exact build (`"1.2.3"`), or `"current"` to restore the
  inputs recorded for this checkout's completed build (see
  [`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)).
  Defaults to the configured version, or `"latest"`.

- cue:

  Character. `"if-change"` (the default here), `"always"`, `"if-empty"`
  or `"never"`.

- strategy:

  Character. `"sync-diff"`, `"sync-purge"`, `"download-all"` or
  `"download-missing"`.

- inspect:

  Character. `"manifest"`, `"file"` or `"none"`.

- conflict:

  Character. `"error"`, `"local"` (keep changed local files) or
  `"remote"` (replace them).

## Value

Invisibly returns `TRUE`.

## See also

[`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md),
[`projr_get_local()`](https://satvilab.github.io/projr/reference/projr_get.md),
[`projr_get_github()`](https://satvilab.github.io/projr/reference/projr_get.md)

## Examples

``` r
if (FALSE) { # \dontrun{
projr_source()
projr_source("raw-survey", remote = "github:survey-team", version = "0.6")
projr_source(conflict = "remote")
} # }
```
