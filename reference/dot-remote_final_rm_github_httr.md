# Delete a release asset using httr

Deletes an asset from a GitHub release by filename.

## Usage

``` r
.remote_final_rm_github_httr(repo, tag, fn, api_url = NULL, token = NULL)
```

## Arguments

- repo:

  Character string. Repository in format "owner/repo".

- tag:

  Character string. Release tag.

- fn:

  Character string. Filename of the asset to delete.

- api_url:

  Character string. Optional GitHub API URL.

- token:

  Character string. Optional GitHub token.

## Value

Logical. TRUE if successful.
