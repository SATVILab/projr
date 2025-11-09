# Set Project Version

Sets the project version manually in the `DESCRIPTION` file and
optionally in a `VERSION` file. This is useful in cases where you need
to increment the version manually, for example, if a collaborator has
pushed changes and you want to manually set your version before merging.

## Usage

``` r
projr_version_set(version, only_if_exists = TRUE)
```

## Arguments

- version:

  A character string specifying the version to set. It may include a
  development component (e.g., "1.2.3-dev") or just the stable version
  (e.g., "1.2.3").

- only_if_exists:

  A logical flag indicating whether to update the `VERSION` file only if
  it already exists (`TRUE`) or to create it if it doesn't exist
  (`FALSE`). Defaults to `TRUE`.

## Value

Invisibly returns `TRUE` if successful.
