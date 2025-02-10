# Helper function to retrieve the entire Quarto YAML configuration
.projr_yml_quarto_get <- function() {
  # Construct the path to the `_quarto.yml` file within the project directory
  path_yml <- .path_get("_quarto.yml")
  
  # Check if the YAML file exists; if not, return an empty list
  if (!file.exists(path_yml)) {
    return(list())
  }
  
  # Read and parse the YAML file into an R list using the yaml package
  yaml::read_yaml(path_yml)
}

# Helper function to write updates to the Quarto YAML configuration
.projr_yml_quarto_set <- function(list_save) {
  # Construct the path to the `_quarto.yml` file within the project directory
  path_yml <- .path_get("_quarto.yml")
  
  # Ensure that specific YAML keys are stored as lists rather than strings
  # This is necessary because Quarto expects certain fields to be lists
  list_save <- .wrap_in_list_if_string(list_save, c("project", "render"))
  
  # Ensure 'website' related keys are lists
  list_save <- .wrap_in_list_if_string(list_save, c("website", "other-links"))
  list_save <- .wrap_in_list_if_string(list_save, c("website", "code-links"))
  list_save <- .wrap_in_list_if_string(list_save, c("website", "navbar", "right"))
  list_save <- .wrap_in_list_if_string(list_save, c("website", "navbar", "left"))
  
  # TODO: Extend handling for additional `website` navigation items as needed
  
  # Ensure 'book' related keys are lists
  list_save <- .wrap_in_list_if_string(list_save, c("book", "chapters"))
  list_save <- .wrap_in_list_if_string(list_save, c("book", "appendices"))
  
  # Placeholder for handling 'manuscript' specific keys in the future
  # ...
  
  # Write the modified YAML back to the `_quarto.yml` file with custom handlers
  # The handler ensures that logical values are written as lowercase 'true'/'false'
  yaml::write_yaml(
    list_save, path_yml,
    handlers = list(logical = function(x) {
      value <- ifelse(x, "true", "false")
      structure(value, class = "verbatim")
    })
  )
  
  # Append a newline to the YAML file to ensure proper formatting
  .projr_newline_append(path_yml)
  
  # Return TRUE invisibly to indicate that the operation was successful
  invisible(TRUE)
}

# Function to get the output directory from the Quarto project configuration
.projr_yml_quarto_get_output_dir <- function() {
  # Access the 'output-dir' field within the 'project' section of the YAML
  .projr_yml_quarto_get_project()[["output-dir"]]
}

# Function to set the output directory in the Quarto project configuration
.projr_yml_quarto_set_output_dir <- function(path) {
  # Retrieve the current YAML configuration as a list
  yml_quarto <- .projr_yml_quarto_get()
  
  # Update the 'output-dir' field within the 'project' section with the new path
  yml_quarto[["project"]][["output-dir"]] <- path
  
  # Write the updated YAML configuration back to the `_quarto.yml` file
  .projr_yml_quarto_set(yml_quarto)
}

# Function to get the project type from the Quarto configuration
.projr_yml_quarto_get_project_type <- function() {
  # Access the 'type' field within the 'project' section of the YAML
  .projr_yml_quarto_get_project()[["type"]]
}

# Function to retrieve the 'project' section from the Quarto YAML
.projr_yml_quarto_get_project <- function() {
  # Access the 'project' key in the entire YAML configuration
  .projr_yml_quarto_get()[["project"]]
}

# Function to get a top-level named element from the Quarto YAML
.projr_yml_quarto_get_nm <- function(nm) {
  # Retrieve the value associated with the specified top-level key `nm` in the YAML
  .projr_yml_quarto_get()[[nm]]
}

# Helper function to wrap a nested element in a list if it’s a string
.wrap_in_list_if_string <- function(lst, path) {
  # Retrieve the current value at the specified nested path within the list
  val <- .get_nested_value(lst, path)
  
  # Check if the retrieved value is a string
  if (.is_string(val)) {
    # If it's a string, wrap it in a one-element list to conform to YAML structure
    lst <- .set_nested_value(lst, path, list(val))
  }
  
  # Return the potentially modified list
  lst
}

# Safely retrieve a value at a nested path within a list
.get_nested_value <- function(x, keys) {
  # Iterate through each key in the path to traverse the nested list structure
  for (k in keys) {
    # If the current element is not a list or the key does not exist, return NULL
    if (!is.list(x) || is.null(x[[k]])) return(NULL)
    
    # Move deeper into the nested list by updating `x` to the next level
    x <- x[[k]]
  }
  
  # Return the final retrieved value after traversing all keys
  x
}

# Assign a value at a nested path within a list
.set_nested_value <- function(x, keys, value) {
  # If there's only one key in the path, assign the value directly to that key
  if (length(keys) == 1) {
    x[[keys]] <- value
  } else {
    # Otherwise, recursively set the value in the deeper nested list
    # This ensures that all intermediate lists are appropriately updated
    x[[keys[1]]] <- .set_nested_value(x[[keys[1]]], keys[-1], value)
  }
  
  # Return the modified list with the new value set
  x
}
