# Manually Ignore Files or Directories in `.gitignore` and `.Rbuildignore`

These functions allow manual addition of files and directories to the
`.gitignore` and `.Rbuildignore` files, outside of the automatic
management provided by the `projr` package.

- .ignore_manual`: General function to add both files and directories to both `.gitignore`and`.Rbuildignore\`.
  If a path does not exist, it is treated as a file.

- .ignore_manual_dir`: Specifically adds directories to both `.gitignore`and`.Rbuildignore\`.

- .ignore_manual_file`: Specifically adds files to both `.gitignore`and`.Rbuildignore\`.

- .ignore_manual_dir_git` and .ignore_manual_file_git`: Add directories
  or files explicitly to `.gitignore`.

- .ignore_manual_dir_rbuild` and .ignore_manual_file_rbuild`: Add
  directories or files explicitly to `.Rbuildignore`.

## Usage

``` r
projr_ignore(ignore, force_create = TRUE)
```

## Arguments

- ignore:

  A character vector of file or directory paths to be ignored. Paths
  must be valid non-empty strings.

- force_create:

  logical. If `FALSE`, then the function will only add to the
  corresponding ignore file (`.gitignore`/`.Rbuildignore`) if it already
  exists, OR if it is warranted (i.e. if there is a Git repository or
  DESCRIPTION file, respectively). If `TRUE`, then the function will
  create the ignore file if it does not exist, even if there is no Git
  repository or DESCRIPTION file, and will add the specified paths to
  it. Default is `TRUE`.

## Value

Invisibly returns `TRUE` if the operation succeeds, or `FALSE` if the
input contains invalid (empty) paths.

## Details

These functions provide fine-grained control for cases where users want
to manually ignore specific paths permanently. They do not interact with
the automated ignore management system of `projr`.

- Non-existent paths provided to .ignore_manual\` are assumed to be
  files.

- For `.gitignore`, directories are automatically appended with `/**` if
  missing, ensuring proper Git ignore syntax.

- For `.Rbuildignore`, paths are converted to regular expressions using
  `glob2rx` for compatibility with R's build tools.

## See also

.ignore_auto` for dynamically managed ignore entries, and .unignore_manual`
for forcing certain paths to not be ignored.

## Examples

``` r
# Manually ignore files and directories
projr_ignore_manual(c("output", "tempfile.log"))
#> Error in projr_ignore_manual(c("output", "tempfile.log")): could not find function "projr_ignore_manual"

# Specifically ignore directories
projr_ignore_dir("data")

# Specifically ignore files
projr_ignore_file("README.md")
```
