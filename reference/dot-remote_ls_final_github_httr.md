# Get release information by tag using httr

Fetches release information from GitHub API using the tag name.

## Usage

``` r
.remote_ls_final_github_httr(repo, tag, api_url = NULL, token = NULL)
```

## Arguments

- repo:

  Character string. Repository in format "owner/repo".

- tag:

  Character string. Release tag.

- api_url:

  Character string. Optional GitHub API URL.

- token:

  Character string. Optional GitHub token.

## Value

List containing release information from GitHub API.
