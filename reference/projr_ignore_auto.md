# Update `.gitignore` and `.Rbuildignore` with projr-managed ignores

The
projr_ignore_auto()`function updates the project’s`.gitignore`and`.Rbuildignore\`
files to reflect directories and files managed by projr, as well as
other directories and files that should clearly be ignored. They are
kept up-to-date with the project's configuration, and are written within
a demarcated section of the file.

## Usage

``` r
projr_ignore_auto()
```

## Value

Called primarily for its side effects (modifying `.gitignore` and/or
`.Rbuildignore`). Returns `TRUE` invisibly.

## See also

.ignore_add.ignore_add_git.ignore_add_rbuild

## Examples

``` r
if (FALSE) { # \dontrun{
projr_ignore_auto()
} # }
```
