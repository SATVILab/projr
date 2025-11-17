# Check OSF authentication

Checks if OSF authentication is available. Throws an error if not. Used
internally before making OSF API calls.

## Usage

``` r
.auth_check_osf(context = NULL)
```

## Arguments

- context:

  Character string describing the operation context for error messages.
  Default is NULL.

## Value

Invisible TRUE if authentication is available.
