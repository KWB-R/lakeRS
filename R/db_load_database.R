#' Load a database table
#' 
#' @param ndtriPath Path of the NDTrI data which includes the _database folder 
#' @param dbName Character string
#' 
#' @return 
#' Function reads a csv file from RSPath/_database folder
#' 
#' @export
#' 
load_database <- function(ndtriPath, dbName){
  dp <- file.path(ndtriPath, "_database")
  all_db <- dir(dp)
  if(paste0(dbName, ".csv") %in% dir(dp)){
    read.csv(
      file = file.path(dp, paste0(dbName, ".csv")), 
      colClasses = c("lakeID" = "character"),
      header = TRUE, 
      sep = ";", 
      dec = "."
    )
  } else {
    stop("No database named ", dbName, " in ", dp )
  }
}
