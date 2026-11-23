# Set Git options

`projr_yml_git_set()` sets Git options for the project.

The options are:

- commit: whether to commit changes automatically upon project builds.

- add_untracked: whether to add untracked files automatically upon
  project builds.

- push: whether to push changes automatically upon project builds.

The default is to leave all the settings unchanged.

If these settings are absent from `_projr.yml`, then the default is to
commit, add untracked files and push.

`projr_yml_git_set_default()` sets all Git options to their default
(`TRUE`).

## Usage

``` r
projr_yml_git_set(
  all = NULL,
  commit = NULL,
  add_untracked = NULL,
  push = NULL,
  simplify_identical = TRUE,
  simplify_default = TRUE,
  profile = "default"
)

projr_yml_git_set_default(
  profile = "default",
  simplify_identical = TRUE,
  simplify_default = TRUE
)
```

## Arguments

- all:

  logical. Whether to set all the options to `TRUE` or `FALSE`. If
  `NULL`, then `commit`, `add_untracked` and `push` are used. Default is
  `NULL`.

- commit:

  logical. Whether to commit changes automatically upon project builds.
  If `NULL`, then setting is not changed. Default is `NULL`.

- add_untracked:

  logical. Whether to add untracked files automatically upon project
  builds. If `NULL`, then setting is not changed. Default is `NULL`.

- push:

  logical. Whether to push changes automatically upon project builds. If
  `NULL`, then setting is not changed. Default is `NULL`.

- simplify_identical:

  logical. If `TRUE`, then if all the settings are the same (for
  `commit`, `push` and `add_untracked`), then only `git: TRUE` or
  `git: FALSE` is written to `_projr.yml`. Default is `TRUE`.

- simplify_default:

  logical. If `TRUE`, then if the settings are the same as the default
  (which is TRUE), then the settings are removed from `_projr.yml`.
  Default is `TRUE`.

- profile:

  Profile to update. The default, `"default"`, updates `_projr.yml`. A
  named profile updates `_projr-<profile>.yml`. With `NULL`, read the
  merged active settings and write to `_projr.yml`.

## Examples

``` r
if (FALSE) { # \dontrun{
# set all to TRUE
projr_yml_git_set(all = TRUE)

# set all to FALSE
projr_yml_git_set(all = FALSE)

# set only add_untracked to FALSE
projr_yml_git_set(add_untracked = FALSE)

# revert to defaults
projr_yml_git_set_default()
} # }
```
