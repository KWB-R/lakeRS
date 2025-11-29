#' Creates a new database table
#' 
#' @param ndtriPath Path of the NDTrI data which includes all the assessed lakes 
#' as folders
#' @param included_folders Subfolders or the output folder which are part of the
#' new databse
#' @param dbName Character string
#' 
#' @return 
#' Function saves a csv file in the RSPath/_database folder
#' 
#' @export
#' 
create_database <- function(ndtriPath, included_folders, dbName){
  output <- lapply(included_folders, function(f){
    if(file.exists(file.path(ndtriPath, f, "_summary.csv"))){
      read.csv(
        file = file.path(ndtriPath, f, "_summary.csv"),
        header = TRUE, 
        sep = ";",
        dec = "."
      )
    } else {
      NULL
    }
  })
  
  output <- do.call(rbind, output)
  dp <- file.path(ndtriPath, "_database")
  if(!dir.exists(dp)){
    dir.create(dp)
  }
  write.table(
    x = output, 
    file = file.path(dp, paste0(dbName, ".csv")), 
    sep = ";", dec = ".", row.names = FALSE)
}