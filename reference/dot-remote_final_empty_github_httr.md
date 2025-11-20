# Delete a release asset using httr

Deletes an asset from a GitHub release by asset ID.

## Usage

``` r
.remote_final_empty_github_httr(repo, tag, fn, api_url = NULL, token = NULL)
```

## Arguments

- repo:

  Character string. Repository in format "owner/repo".

- api_url:

  Character string. Optional GitHub API URL.

- token:

  Character string. Optional GitHub token.

- asset_id:

  Numeric. Asset ID to delete.

## Value

Logical. TRUE if successful.
