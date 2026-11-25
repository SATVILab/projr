# Clone a project and fetch its directories

`projr_restore_repo()` clones a GitHub repository and fetches its
directories with
[`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md).
`projr_restore_repo_wd()` clones into the current directory.

## Usage

``` r
projr_restore_repo(repo, path = NULL, label = NULL, remote = NULL)

projr_restore_repo_wd(repo, label = NULL, remote = NULL)
```

## Arguments

- repo:

  character. GitHub repository, as `"owner/repo"`, or `"repo"` for one
  owned by the authenticated user.

- path:

  character. Directory to clone into. Defaults to the repository name.

- label:

  Character. Directory labels to fetch. Defaults to each source's
  selection, which is every declared raw directory unless `build.source`
  lists content.

- remote:

  Character. A remote registered under `remotes`, named by its title or
  as `<type>:<title>` when the title is used by both types.

## Value

Invisibly returns `TRUE` after fetching, or `FALSE` with a message if
the clone or fetch fails.

## See also

[`projr_source()`](https://satvilab.github.io/projr/reference/projr_source.md),
[`projr_restore()`](https://satvilab.github.io/projr/reference/projr_restore.md)

## Examples

``` r
if (FALSE) { # \dontrun{
projr_restore_repo("owner/repo")
projr_restore_repo("owner/repo", label = "raw-data", remote = "github:archive")
} # }
```
