# Test renv restore

Tests
[`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)
without using the cache in a clean, temporary environment. Automatically
creates a temporary project directory, initializes `renv`, copies
required files, disables the cache via `.Rprofile`, and then performs
[`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html).
Afterwards, it deletes the first library path where `renv` restored
packages.

**Note:** To ensure isolation, the test runs in a directory that is
completely separate from the parent project and executes `Rscript` with
the `--vanilla` option. The `--vanilla` flag seems essential to prevent
the project `renv` settings from being affected by testing.

## Usage

``` r
projr_renv_test(files_to_copy = NULL, delete_lib = TRUE)
```

## Arguments

- files_to_copy:

  character vector. Paths to files to copy into the temporary directory
  before restoring. Note that `renv.lock` is always copied.

- delete_lib:

  Logical. If `TRUE`, the restored library path is deleted after the
  test. Default is `TRUE`.

## Value

`TRUE` if
[`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)
succeeds, `FALSE` otherwise.
