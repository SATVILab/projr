# Set build restrictions

`projr_yml_restrictions_set` sets build restrictions in `_projr.yml`.

The options are:

- branch: controls which branches can perform builds. If `TRUE`
  (default), builds are allowed on any branch. If a character vector,
  builds are only allowed on matching branches. If `FALSE`, builds are
  restricted (treated as empty character vector).

- not_behind: whether to check if branch is behind remote upstream. If
  `TRUE` (default), build fails if branch is behind remote. If `FALSE`,
  no check is performed.

## Usage

``` r
projr_yml_restrictions_set(
  branch = NULL,
  not_behind = NULL,
  profile = "default"
)
```

## Arguments

- branch:

  logical or character. Controls which branches can perform builds. If
  `TRUE`, builds allowed on any branch (default). If a character vector,
  builds only allowed on these branches. If `FALSE`, builds restricted
  on all branches. If `NULL`, setting is not changed. Default is `NULL`.

- not_behind:

  logical. Controls whether to check if branch is behind remote
  upstream. If `TRUE` (default), build fails if branch is behind. If
  `FALSE`, no check is performed. If `NULL`, setting is not changed.
  Default is `NULL`.

- profile:

  character. The profile to write to. Default is "default", in which
  case it writes to `_projr.yml`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Allow builds on any branch (default)
projr_yml_restrictions_set(branch = TRUE)

# Allow builds only on main and dev branches
projr_yml_restrictions_set(branch = c("main", "dev"))

# Restrict builds on all branches
projr_yml_restrictions_set(branch = FALSE)

# Disable check for being behind remote
projr_yml_restrictions_set(not_behind = FALSE)

# Enable check for being behind remote (default)
projr_yml_restrictions_set(not_behind = TRUE)
} # }
```
