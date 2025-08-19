#' Load a database table
#' 
#' @param RSPath Path of remoate sensing input and output folders
#' @param dbName Character string
#' 
#' @return 
#' Function reads a csv file from RSPath/_database folder
#' 
#' @export
#' 
load_database <- function(RSPath, dbName){
  dp <- file.path(RSPath, "output", "_database")
  all_db <- dir(dp)
  if(paste0(dbName, ".csv") %in% dir(dp)){
    read.csv(
      file = file.path(dp, paste0(dbName, ".csv")), 
      header = TRUE, 
      sep = ";", 
      dec = "."
    )
  } else {
    stop("No database named ", dbName, " in ", dp )
  }
}
