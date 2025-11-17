# Check GitHub authentication

Checks if GitHub authentication is available. Throws an error if not.
Used internally before making GitHub API calls.

## Usage

``` r
.auth_check_github(context = NULL)
```

## Arguments

- context:

  Character string describing the operation context for error messages.
  Default is NULL.

## Value

Invisible TRUE if authentication is available.
