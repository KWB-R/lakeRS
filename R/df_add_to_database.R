#' Add a new lake to the database
#' 
#' @param RSPath Path of remoate sensing input and output folders
#' @param dbName Name of the dabase
#' @param folder A folder Name of the data to be added.  On of folder or df 
#' needs to be defined
#' @param df A dataframe to be added. On of folder or df needs to be defined
#' 
#' @return 
#' Function saves the extended database table as csv
#' 
#' @export
#' 
add_to_database <- function(RSPath,  dbName, folder = NULL, df = NULL){
  op <- file.path(RSPath, "output")

  db <- load_database(RSPath = RSPath, dbName = dbName)
  
  if(!is.null(folder)){
    lake_data <- read.csv(
      file = file.path(op, folder, "_summary.csv"),
      header = TRUE, 
      sep = ";",
      dec = "."
    )
    
    output <- rbind(db, lake_data)
   
  } else if (!is.null(df)){
    output <- rbind(db, df)
  } else {
    stop("Either the 'folder' or the 'df' argument needs to be defined")
  }
  
  not_added <- duplicated(output[,c("lakeName", "lakeID", "year")])
  if(sum(not_added) > 0L){
    warning(
      "Values of lake ", unique(output$lakeName[not_added]), " in ", 
      paste0(output$year[not_added], collapse = ", "), 
      " have already been part of the database and were not added.")
  }
  output <- output[!not_added,]
  
  write.table(
    x = output, 
    file = file.path(op, "_database", paste0(dbName, ".csv")), 
    sep = ";", dec = ".", row.names = FALSE)
  
  output
}

