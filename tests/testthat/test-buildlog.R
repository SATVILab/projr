# Test buildlog system resources functionality

test_that(".buildlog_get_system_resources works", {
  skip_if(.is_test_select())
  
  # Get system resources
  sys_resources <- .buildlog_get_system_resources()
  
  # Check that it returns a character vector
  expect_true(is.character(sys_resources))
  
  # Check that it contains expected sections
  expect_true(any(grepl("\\*\\*System Resources\\*\\*", sys_resources)))
  expect_true(any(grepl("- OS:", sys_resources)))
  expect_true(any(grepl("- CPU Cores:", sys_resources)))
  expect_true(any(grepl("- Total RAM:", sys_resources)))
  expect_true(any(grepl("- Disk Space:", sys_resources)))
  expect_true(any(grepl("- Architecture:", sys_resources)))
  expect_true(any(grepl("- Platform:", sys_resources)))
  
  # Check that CPU cores is a positive number
  cpu_line <- sys_resources[grepl("- CPU Cores:", sys_resources)]
  cpu_cores <- as.numeric(gsub(".*: ", "", cpu_line))
  expect_true(cpu_cores > 0)
})

test_that(".buildlog_get_memory_info works", {
  skip_if(.is_test_select())
  
  # Get memory info
  mem_info <- .buildlog_get_memory_info()
  
  # Check that it returns a character string
  expect_true(is.character(mem_info))
  expect_true(length(mem_info) == 1)
  
  # Check that it contains RAM information
  expect_true(grepl("- Total RAM:", mem_info))
})

test_that(".buildlog_get_disk_info works", {
  skip_if(.is_test_select())
  
  # Get disk info
  disk_info <- .buildlog_get_disk_info()
  
  # Check that it returns a character string
  expect_true(is.character(disk_info))
  expect_true(length(disk_info) == 1)
  
  # Check that it contains disk space information
  expect_true(grepl("- Disk Space:", disk_info))
})

test_that(".buildlog_get_add includes system resources", {
  skip_if(.is_test_select())
  
  # Create sample inputs
  msg <- "Test build message"
  bump_component <- "patch"
  version_run_on_list <- list(
    "desc" = list("success" = "0.0.1")
  )
  total_time <- as.difftime(120, units = "secs")
  
  # Get the buildlog addition
  add_txt <- .buildlog_get_add(msg, bump_component, version_run_on_list, total_time)
  
  # Check that it includes system resources section
  expect_true(any(grepl("\\*\\*System Resources\\*\\*", add_txt)))
  expect_true(any(grepl("- OS:", add_txt)))
  expect_true(any(grepl("- CPU Cores:", add_txt)))
})
