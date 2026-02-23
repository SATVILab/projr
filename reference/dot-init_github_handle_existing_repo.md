# Handle existing GitHub repository

Checks if a GitHub repository exists and prompts user to set it as
remote if it does. In non-interactive contexts, throws an error.

## Usage

``` r
.init_github_handle_existing_repo(owner, repo)
```

## Arguments

- owner:

  Character string. Repository owner (username or organization).

- repo:

  Character string. Repository name.

## Value

Invisible TRUE if repository doesn't exist or user chose to set remote.
Otherwise stops with error.
