# `projr` drop-in replacement for usethis::use_data

Save package datasets in the staged build cache by default. For datasets
intended for package users, declare the `data` label under `directories`
with the final path (normally `data` in an R package). Internal datasets
use the package's `R/sysdata.rda` convention.

## Usage

``` r
projr_use_data(
  ...,
  internal = FALSE,
  overwrite = FALSE,
  compress = "bzip2",
  version = 2,
  ascii = FALSE,
  safe = TRUE
)
```

## Arguments

- ...:

  Unquoted names of existing objects to save.

- internal:

  If `FALSE`, saves each object in its own `.rda` file in the `data/`
  directory. These data files bypass the usual export mechanism and are
  available whenever the package is loaded (or via
  [`data()`](https://rdrr.io/r/utils/data.html) if `LazyData` is not
  true).

  If `TRUE`, stores all objects in a single `R/sysdata.rda` file.
  Objects in this file follow the usual export rules. Note that this
  means they will be exported if you are using the common
  `exportPattern()` rule which exports all objects except for those that
  start with `.`.

- overwrite:

  By default,
  .use_data()`will not overwrite existing files. If you really want to do so, set this to`TRUE\`.

- compress:

  Choose the type of compression used by
  [`save()`](https://rdrr.io/r/base/save.html). Should be one of "gzip",
  "bzip2", or "xz".

- version:

  The serialization format version to use. The default, 2, was the
  default format from R 1.4.0 to 3.5.3. Version 3 became the default
  from R 3.6.0 and can only be read by R versions 3.5.0 and higher.

- ascii:

  if `TRUE`, an ASCII representation of the data is written. The default
  value of `ascii` is `FALSE` which leads to a binary file being
  written. If `NA` and `version >= 2`, a different ASCII representation
  is used which writes double/complex numbers as binary fractions.

- safe:

  logical. Whether to save data to a temporary directory (in
  `<cache>/"projr"/v<version>/data/`) or the configured final data
  directory. Default is the temporary directory (TRUE).

## Details

Taken directly from the documentation for the original `usethis`
function, and adjusted slightly.

## See also

The [data chapter](https://r-pkgs.org/data.html) of [R
Packages](https://r-pkgs.org).

## Examples

``` r
if (FALSE) { # \dontrun{
x <- 1:10
y <- 1:100

projr_use_data(x, y) # For external use
projr_use_data(x, y, internal = TRUE) # For internal use
} # }
```
