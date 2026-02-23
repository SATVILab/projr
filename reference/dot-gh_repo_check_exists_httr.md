# Check if GitHub repository exists using httr

Uses the GitHub API to check if a repository exists for a given owner.

## Usage

``` r
.gh_repo_check_exists_httr(owner, repo, api_url = NULL, token = NULL)
```

## Arguments

- owner:

  Character string. Repository owner (username or organization).

- repo:

  Character string. Repository name.

- api_url:

  Character string. Optional GitHub API URL for enterprise instances.

- token:

  Character string. Optional GitHub token.

## Value

Logical TRUE/FALSE if repository exists/doesn't exist.
