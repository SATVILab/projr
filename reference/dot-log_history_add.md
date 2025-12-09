# Update build history file

Update build history file

## Usage

``` r
.log_history_add(
  build_type = "output",
  bump_component = NULL,
  msg = "",
  success = TRUE
)
```

## Arguments

- build_type:

  Character. Either "output" or "dev".

- bump_component:

  Character. Version bump component.

- msg:

  Character. Build message.

- success:

  Logical. Whether build succeeded.
