# Set environment variables for projr_init

Set environment variables for `projr_init.` When set, `projr_init` will
use these variables to provide options to populate the metadata. This
function creates the `.Renviron` file for the user if it does not exist.
It then adds the variables to the `.Renviron` file without any values
set.

## Usage

``` r
projr_init_renviron()
```
