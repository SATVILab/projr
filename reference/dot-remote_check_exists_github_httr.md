# Check if a GitHub release exists using httr

Uses the GitHub API directly to check if a release with the given tag
exists. This is faster and more explicit than relying on piggyback
internals.

## Usage

``` r
.remote_check_exists_github_httr(repo, tag, api_url = NULL, token = NULL)
```

## Arguments

- repo:

  Character string. Repository in format "owner/repo".

- tag:

  Character string. Release tag to check.

- api_url:

  Character string. Optional GitHub API URL for enterprise instances.

- token:

  Character string. Optional GitHub token. If not provided, uses
  `.auth_get_github_pat_find()`.

## Value

Logical TRUE/FALSE if release exists/doesn't exist and stops on auth
errors
