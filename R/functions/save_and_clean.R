save_and_clean <- function(object, dir, object_name) {
  # Save full RDS object
  saveRDS(object, file = paste0(dir, "/", object_name, ".RDS"))
  # Clean up individual RDS pieces
  lapply(list.files(path = dir,
                    pattern = paste0(object_name,"[0-9]+.RDS"),
                    recursive = FALSE),
         FUN = function(file_name) unlink(paste0(dir, "/", file_name)))
}