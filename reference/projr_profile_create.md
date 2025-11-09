# Add projr profile file

Creates a new `projr` profile that can override settings in
`_projr.yml`. If the associated file does not exists, it creates a blank
file. The file is ignored from the R build process and, if it is the
`local` profile, from Git as well.

## Usage

``` r
projr_profile_create(profile)
```

## Arguments

- profile:

  character. Name of the profile. If not supplied, then the profile is
  named `default` (and the file `_projr.yml` is created).

## Value

Invisibly returns `TRUE` if the file was created, and `FALSE` if the
file already exists.

## See also

.profile_create_local.profile_get
