# Set restrictions for production builds

Configure which branches may build and how projr checks Git and archive
versions before a production build. Omit a setting or pass `NULL` to
leave its current value in place.

## Usage

``` r
projr_yml_restrictions_set(
  branch = NULL,
  not_behind = NULL,
  profile = "default",
  not_behind_git = NULL,
  not_behind_remote = NULL,
  rule = NULL
)
```

## Arguments

- branch:

  `TRUE` allows any branch (default), a character vector allows the
  named branches, and `FALSE` prevents production builds on any branch.

- not_behind:

  Earlier spelling of `not_behind_git`.

- profile:

  Name of the profile to update. The default is `"default"`.

- not_behind_git:

  Whether to check if the local branch is behind its Git upstream.
  Defaults to `TRUE`.

- not_behind_remote:

  Which archives to check. `TRUE` (default) checks the destinations
  selected for publication by this build. `FALSE` skips archive version
  checks. To check specific destinations, supply a character vector of
  archive titles or a named list such as
  `list(github = "reports", local = "backup")`. Include the type when a
  title applies to more than one archive. Selected archives are checked
  even when the build will not publish to them.

- rule:

  Which project version must be at least as recent as each selected
  archive's completed version. `"base_version_not_behind"` (or `"base"`,
  the default) compares the version before the bump.
  `"target_version_not_behind"` (or `"target"`) compares the version the
  build will create.

## Value

Invisibly returns `TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
projr_yml_restrictions_set(branch = c("main", "dev"))
projr_yml_restrictions_set(not_behind_git = FALSE)
projr_yml_restrictions_set(
  not_behind_remote = list(github = "reports", local = "backup"),
  rule = "target"
)
} # }
```
