# Create a GitHub release using httr

Creates a GitHub release for the given tag using the GitHub REST API.

## Usage

``` r
.gh_release_create_httr(
  repo,
  tag,
  description,
  api_url = NULL,
  token = NULL,
  draft = FALSE,
  prerelease = FALSE,
  target_commitish = NULL
)
```

## Arguments

- repo:

  Character string. Repository in format "owner/repo".

- tag:

  Character string. Release tag.

- description:

  Character string. Release body text.

- api_url:

  Character string. Optional GitHub API URL (for Enterprise).

- token:

  Character string. Optional GitHub token. If not supplied,
  `.auth_get_github_pat_find()` is used.

- draft:

  Logical. Whether the release should be created as a draft.

- target_commitish:

  Character string. Optional target commitish (branch or SHA). If NULL,
  GitHub uses the default branch.

## Value

Parsed release object (list) from GitHub API on success.
