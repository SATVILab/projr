# Upload an asset to a GitHub release using httr

Uploads a file as an asset to an existing GitHub release using the
GitHub API directly.

## Usage

``` r
.gh_release_asset_upload_httr(
  repo,
  tag,
  file_path,
  asset_name = NULL,
  overwrite = TRUE,
  api_url = NULL,
  token = NULL
)
```

## Arguments

- repo:

  Character string. Repository in format "owner/repo".

- tag:

  Character string. Release tag.

- file_path:

  Character string. Path to file to upload.

- asset_name:

  Character string. Optional name for the asset. If NULL, uses basename
  of file_path.

- overwrite:

  Logical. If TRUE and asset exists, deletes it first.

- api_url:

  Character string. Optional GitHub API URL.

- token:

  Character string. Optional GitHub token.

## Value

List containing asset information from GitHub API.
