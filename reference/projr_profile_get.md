# Get active projr profile

Get active `projr` profile(s).

## Usage

``` r
projr_profile_get()
```

## Value

Character vector of length equal to number of active profiles.

## Details

Note that `local` and `default` are not returned, but are always active
(if they exist). `local` corresponds to `_projr-local.yml` and `default`
to `_projr.yml`.
