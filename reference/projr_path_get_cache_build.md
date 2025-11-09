# Get `projr` build cache directory

Get the cache directory for `projr` builds. It is a sub-directory of the
cache directory. For development builds
(.build_dev`), this is the final directory for `output`and`docs` items. For output builds (.build_output`),
this is the staging directory. After the documents are rendered, they
are copied to their final directories.

.path_get_cache_build` assumes the path is to a file, whereas .path_get_cache_build_dir`
assumes the path is to a directory. This distinctiion is only relevant
when `create = TRUE`, as it determines what directory is attempted to be
created.

## Usage

``` r
projr_path_get_cache_build_dir(..., create = FALSE, profile)

projr_path_get_cache_build(..., create = FALSE, profile)
```

## Arguments

- ...:

  comma-separated strings specified initially the label (e.g. `"docs"`
  or `"output"`) as well as, optionally, sub-directories (e.g.
  `"img", "`). For example, .path_get_cache_build("docs",
  "img")`returns the path to the`img`directory in the`docs\`
  sub-directory of the build cache directory.

- create:

  logical. If `TRUE`, then the directory is created if it does not
  exist.

- profile:

  character. The name of the `projr` profile to use. Default is `NULL`,
  which uses the current `projr` profile.

## Value

character. Path to the cache (sub-)directory for `projr` builds.

## See also

.path_get.path_get_dir
