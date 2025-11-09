# Initialise project

Initialise project

## Usage

``` r
projr_init_prompt(y)
```

## Arguments

- yml_path_from:

  character. Path to YAML file to use as `_projr.yml`. If not supplied,
  then default `_projr.yml` file is used.

- renv_force:

  Logical. Passed to
  [`renv::init()`](https://rstudio.github.io/renv/reference/init.html).
  If `FALSE`, then
  [`renv::init()`](https://rstudio.github.io/renv/reference/init.html)
  will not run if it detects that the working directory already is
  registered with renv. Default is `FALSE`.

- renv_bioconductor:

  Logical. Whether `renv` should look for packages on Bioconductor.
  Default is `TRUE`.

- public:

  logical. Whether the GitHub repo created (if any) is public or not.
  Default is `FALSE`.

## See also

.init_renviron
