# Manually Unignore Files or Directories in `.gitignore` and `.Rbuildignore`

These functions allow manual addition of files and directories to the
`.gitignore` and `.Rbuildignore` files **after** the projr-managed
block, thereby forcing them to be *not* ignored.

- .unignore_manual`: General function to unignore both files and directories in both `.gitignore`and`.Rbuildignore\`.
  If a path does not exist, it is treated as a file.

- .unignore_manual_dir`: Specifically unignores directories in both `.gitignore`and`.Rbuildignore\`.

- .unignore_manual_file`: Specifically unignores files in both `.gitignore`and`.Rbuildignore\`.

- .unignore_manual_dir_git` and .unignore_manual_file_git`: Add
  directories or files explicitly (with a `!` prefix) to `.gitignore`.

- .unignore_manual_dir_rbuild` and .unignore_manual_file_rbuild`: Add
  directories or files explicitly (with a `!` prefix) to
  `.Rbuildignore`.

## Usage

``` r
projr_unignore_manual(unignore)

projr_unignore_manual_dir(unignore)

projr_unignore_manual_file(unignore)

projr_unignore_manual_file_git(unignore)

projr_unignore_manual_dir_git(unignore)

projr_unignore_manual_file_rbuild(unignore)

projr_unignore_manual_dir_rbuild(unignore)
```

## Arguments

- unignore:

  A character vector of file or directory paths to be unignored. Paths
  must be valid non-empty strings.

## Value

Invisibly returns `TRUE` if the operation succeeds, or `FALSE` if the
input contains invalid (empty) paths.

## Details

These functions provide fine-grained control for cases where users want
to *undo* any ignoring behavior for specific paths permanently. They do
not interact with the automated ignore management system of `projr`.

- Non-existent paths provided to .unignore_manual\` are assumed to be
  files.

- For `.gitignore`, unignored directories are automatically appended
  with `/**` if missing, then prepended with `!`, ensuring proper Git
  *unignore* syntax.

- For `.Rbuildignore`, paths are converted to regular expressions using
  [`glob2rx()`](https://rdrr.io/r/utils/glob2rx.html), and then
  prepended with `!` for compatibility with R's build tools.

## See also

.ignore_manual` for manually ignoring paths, and .ignore_auto` for
dynamically managed ignore entries.

## Examples

``` r
# Manually unignore files and directories
projr_unignore_manual(c("output", "tempfile.log"))

# Specifically unignore directories
projr_unignore_manual_dir("data")

# Specifically unignore files
projr_unignore_manual_file("README.md")
```
