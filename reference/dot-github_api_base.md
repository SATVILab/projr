# Get GitHub API base URL

Resolves the GitHub API base URL from explicit argument, environment
variable, or defaults to public GitHub API.

## Usage

``` r
.github_api_base(api_url = NULL)
```

## Arguments

- api_url:

  Character string. Explicit API URL to use. Takes precedence over
  environment variable.

## Value

Character string with trailing slashes removed.
