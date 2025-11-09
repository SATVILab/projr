# Create a new project on OSF

This function creates a new project on the Open Science Framework (OSF)
with the specified title, description, and visibility settings.

## Usage

``` r
projr_osf_create_project(title, description, public)
```

## Arguments

- title:

  character. Title of the project.

- description:

  character. Description of the project.

- public:

  logical. Whether the project should be public (TRUE) or private
  (FALSE).

## Value

A character string containing the ID of the newly created project.

## See also

<https://osf.io/> for more information about OSF.

## Examples

``` r
if (FALSE) { # \dontrun{
projr_osf_create_project(
  title = "My New Project",
  description = "This is a description of my new project.",
  public = TRUE # because open science
)
} # }
```
