# Download all assets from a GitHub release using httr

Downloads every asset attached to a GitHub release (identified by tag)
into a local directory. Uses the GitHub API via `httr` and supports
GitHub Enterprise via `api_url`.

## Usage

``` r
.remote_file_get_all_github_httr(
  repo,
  tag,
  fn,
  dest_dir,
  api_url = NULL,
  token = NULL,
  overwrite = TRUE,
  output_level = "std"
)
```

## Arguments

- repo:

  Character string. Repository in format "owner/repo".

- tag:

  Character string. Release tag.

- fn:

  Character string. Asset filename to download.

- dest_dir:

  Character string. Local directory to save assets into. Created if it
  does not exist.

- api_url:

  Character string. Optional GitHub API URL.

- token:

  Character string. Optional GitHub token. If not supplied,
  `.auth_get_github_pat_find()` is used.

- overwrite:

  Logical. If FALSE, existing files are left untouched.

- output_level:

  Character. Verbosity control passed to
  [`.cli_debug()`](https://satvilab.github.io/projr/reference/dot-cli_debug.md).

## Value

Character vector of downloaded file paths (invisibly).
