# Check if required packages for build are available

Checks if all packages required for the current project build are
installed. Returns structured information about missing packages and
installation commands.

This is particularly useful in CI/CD environments where the installation
commands need to be captured and executed programmatically.

## Usage

``` r
projr_build_check_packages(profile = NULL)
```

## Arguments

- profile:

  character. `projr` profile to use. Default is NULL (use current
  profile).

## Value

A list with the following elements:

- available:

  Logical. TRUE if all required packages are installed, FALSE otherwise.

- missing:

  Character vector of missing package names. Empty if all packages are
  available.

- install_cmds:

  Character vector of R commands to install missing packages. Empty if
  all packages are available.

- message:

  Character. Human-readable message about package status.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check packages for current project
pkg_status <- projr_build_check_packages()

if (!pkg_status$available) {
  cat("Missing packages:", paste(pkg_status$missing, collapse = ", "), "\n")
  cat("Install commands:\n")
  cat(paste(pkg_status$install_cmds, collapse = "\n"), "\n")

  # In CI/CD, you could execute:
  # for (cmd in pkg_status$install_cmds) {
  #   eval(parse(text = cmd))
  # }
}
} # }
```
