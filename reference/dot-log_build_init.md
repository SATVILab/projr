# Initialize a build log file

Initialize a build log file

## Usage

``` r
.log_build_init(
  build_type = "output",
  bump_component = NULL,
  msg = "",
  output_level = "std"
)
```

## Arguments

- build_type:

  Character. Either "output" or "dev".

- bump_component:

  Character. Version bump component.

- msg:

  Character. Build message.

- output_level:

  Character. Output level for this build.

## Value

List with log_file path and timestamp, or NULL if logging disabled.
