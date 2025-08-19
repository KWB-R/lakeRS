#' Creates a new database table
#' 
#' @param RSPath Path of remoate sensing input and output folders
#' @param included_folders Subfolders or the output folder which are part of the
#' new databse
#' @param dbName Character string
#' 
#' @return 
#' Function saves a csv file in the RSPath/_database folder
#' 
#' @export
#' 
create_database <- function(RSPath, included_folders, dbName){
  op <- file.path(RSPath, "output")
  output <- lapply(included_folders, function(f){
    read.csv(
      file = file.path(op, f, "_summary.csv"),
      header = TRUE, 
      sep = ";",
      dec = "."
    )
  })
  
  output <- do.call(rbind, output)
  dp <- file.path(op, "_database")
  if(!dir.exists(dp)){
    dir.create(dp)
  }
  write.table(
    x = output, 
    file = file.path(dp, paste0(dbName, ".csv")), 
    sep = ";", dec = ".", row.names = FALSE)
}