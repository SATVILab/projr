# Restore or Update renv Lockfile Packages

Functions to manage the restoration and updating of packages specified
in the `renv` lockfile.

- .renv_restore()\`: Restores packages from the lockfile, attempting to
  install the lockfile versions.

- .renv_update()\`: Updates packages to their latest available versions,
  ignoring the lockfile versions.

- .renv_restore_and_update()\`: First restores packages from the
  lockfile, then updates them to the latest versions.

## Usage

``` r
projr_renv_restore(
  github = TRUE,
  non_github = TRUE,
  biocmanager_install = FALSE
)

projr_renv_update(
  github = TRUE,
  non_github = TRUE,
  biocmanager_install = FALSE
)

projr_renv_restore_and_update(
  github = TRUE,
  non_github = TRUE,
  biocmanager_install = FALSE
)
```

## Arguments

- github:

  Logical. Whether to process GitHub packages. Default is `TRUE`.

- non_github:

  Logical. Whether to process non-GitHub packages (CRAN and
  Bioconductor). Default is `TRUE`.

- biocmanager_install:

  Logical. If `TRUE`, Bioconductor packages will be installed using
  [`BiocManager::install`](https://bioconductor.github.io/BiocManager/reference/install.html);
  otherwise, `renv::install("bioc::<package_name>")` will be used.
  Default is `FALSE`.

## Value

Invisibly returns `TRUE` upon successful completion.

## Details

Control whether to process GitHub packages, non-GitHub packages (CRAN
and Bioconductor), or both using the `github` and `non_github`
arguments.

## Examples

``` r
if (FALSE) { # \dontrun{
# Restore all packages
projr_renv_restore()

# Update all packages
projr_renv_update()

# Restore and then update all packages
projr_renv_restore_and_update()

# Only restore non-GitHub packages
projr_renv_restore(github = FALSE)

# Only update GitHub packages
projr_renv_update(non_github = FALSE)
} # }
```
