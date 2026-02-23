# Delete a GitHub release using httr

Delete a GitHub release using httr

## Usage

``` r
.remote_rm_github_httr(repo, tag, api_url = NULL, token = NULL)
```

## Arguments

- repo:

  Character string. Repository in format "owner/repo".

- tag:

  Character string. Release tag to delete.

- api_url:

  Optional GitHub API base URL.

- token:

  Optional GitHub token.

## Value

Logical TRUE on success, FALSE if not found.
