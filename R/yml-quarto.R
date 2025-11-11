# Helper function to retrieve the entire Quarto YAML configuration
.yml_quarto_get <- function() {
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
.yml_quarto_set <- function(list_save) {
  # Construct the path to the `_quarto.yml` file within the project directory
  path_yml <- .path_get("_quarto.yml")

  # Ensure that specific YAML keys are stored as lists rather than strings
  # This is necessary because Quarto expects certain fields to be lists
  list_save <- .wrap_in_list_if_string(list_save, c("project", "render"))

  # Ensure 'website' related keys are lists
  list_save <- .wrap_in_list_if_string(list_save, c("website", "other-links"))
  list_save <- .wrap_in_list_if_string(list_save, c("website", "code-links"))
  list_save <- .wrap_in_list_if_string(
    list_save, c("website", "navbar", "right")
  )
  list_save <- .wrap_in_list_if_string(
    list_save, c("website", "navbar", "left")
  )

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
  .newline_append(path_yml)

  # Return TRUE invisibly to indicate that the operation was successful
  invisible(TRUE)
}

# Function to get the output directory from the Quarto project configuration
.yml_quarto_get_output_dir <- function() {
  # Access the 'output-dir' field within the 'project' section of the YAML
  .yml_quarto_get_project()[["output-dir"]]
}

# Main function to set the output directory in the Quarto project configuration
.yml_quarto_set_output_dir <- function(path) {
  # Define the path to your _quarto.yml file
  path_yml_quarto <- .path_get("_quarto.yml")

  # Read the YAML file into R as lines
  yml_lines <- readLines(path_yml_quarto, warn = FALSE)

  # Look for a line that starts with "project:"
  proj_line_ind <- grep("^project:", yml_lines)

  # Update yml_lines based on whether the "project:" key exists
  yml_lines <- if (length(proj_line_ind) == 0) {
    c("project:", paste0("  output-dir: ", path), yml_lines)
  } else {
    .yml_quarto_set_output_dir_present(path, yml_lines, proj_line_ind)
  }

  # Write the modified lines back to the file
  writeLines(yml_lines, path_yml_quarto)
}

# Main function for when the "project:" key is present.
# This function now uses three sub-functions.
.yml_quarto_set_output_dir_present <- function(path,
                                               yml_lines,
                                               proj_line_ind) {
  # 1. Get project block indices
  block_indices <- .yml_quarto_get_project_block_indices(
    yml_lines, proj_line_ind
  )

  # 2. Extract the project block and update it with the new output-dir value
  project_block <- yml_lines[block_indices$start:block_indices$end]
  updated_project_block <- .yml_quarto_update_project_block(
    project_block, path
  )

  # 3. Rebuild the complete YAML lines with the updated project block
  .yml_quarto_rebuild_yaml(
    yml_lines, updated_project_block, block_indices
  )
}

# Sub-function 1: Get the indices (start and end) of the "project:" block.
.yml_quarto_get_project_block_indices <-
  function(yml_lines, proj_line_ind) {
    # Use the first occurrence of "project:"
    proj_start <- proj_line_ind[1]

    # Determine the end of the project block.
    # The block ends when a line does not begin with whitespace.
    proj_end <- length(yml_lines)
    if (proj_start < length(yml_lines)) {
      for (i in (proj_start + 1):length(yml_lines)) {
        if (!grepl("^\\s", yml_lines[i])) {
          proj_end <- i - 1
          break
        }
      }
    }

    list(start = proj_start, end = proj_end)
  }

# Sub-function 2: Update the project block with the new output-dir.
.yml_quarto_update_project_block <- function(project_block, path) {
  output_dir_ind <- which(grepl("^\\s*output-dir:", project_block))

  if (length(output_dir_ind) > 0) {
    # Replace the first occurrence (preserving a two-space indent)
    project_block[output_dir_ind[1]] <- paste0("  output-dir: ", path)
  } else {
    # Append a new output-dir line if not present
    project_block <- c(project_block, paste0("  output-dir: ", path))
  }

  project_block
}

# Sub-function 3: Rebuild the complete YAML file using
# the updated project block.
.yml_quarto_rebuild_yaml <- function(yml_lines,
                                     updated_project_block,
                                     block_indices) {
  start_index <- block_indices$start
  end_index <- block_indices$end

  yml_lines_start <- if (start_index > 1) {
    yml_lines[1:(start_index - 1)]
  } else {
    character(0)
  }
  yml_lines_end <- if (end_index < length(yml_lines)) {
    yml_lines[(end_index + 1):length(yml_lines)]
  } else {
    character(0)
  }

  c(yml_lines_start, updated_project_block, yml_lines_end)
}


# Function to get the project type from the Quarto configuration
.yml_quarto_get_project_type <- function() {
  # Access the 'type' field within the 'project' section of the YAML
  .yml_quarto_get_project()[["type"]]
}

# Function to retrieve the 'project' section from the Quarto YAML
.yml_quarto_get_project <- function() {
  # Access the 'project' key in the entire YAML configuration
  .yml_quarto_get()[["project"]]
}

# Function to get a top-level named element from the Quarto YAML
.yml_quarto_get_nm <- function(nm) {
  # Retrieve the value associated with the specified top-level key `nm` in the YAML
  .yml_quarto_get()[[nm]]
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
    if (!is.list(x) || is.null(x[[k]])) {
      return(NULL)
    }

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
