# unsaved_condition_finder(): Given a directory containing .RDS saved conditions
# and a list of conditions that need to be completed, extracts the completed
# condition numbers and returns the condition numbers that are still incomplete.
unsaved_condition_finder <- function(dir, conditions) {
  # Find all saved files in a directory
  saved_files <- list.files(path = dir)
  # Extract condition numbers from saved files
  saved_conditions <- stringr::str_extract(saved_files, pattern = "[0-9]+")
  saved_conditions <- as.integer(saved_conditions)
  # Return numbers of conditions that didn't get saved
  conditions[!(conditions %in% saved_conditions)]
}