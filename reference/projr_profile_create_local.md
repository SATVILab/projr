# Create a local `projr` profile

Create a `projr` profile with highest precedence (i.e. its settings
overwrite any others) that is ignored by Git.

## Usage

``` r
projr_profile_create_local()
```

## Details

Note that if any setting in `_projr-local.yml` is empty, then a
lower-precendence file's setting (i.e. from `_projr-<profile>.yml` or
`_projr.yml`) is used. Empty settings are by default indicated by `~`.

## See also

.profile_create_local.yml_get
