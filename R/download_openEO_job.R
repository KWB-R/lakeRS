#' Downloads an openEO job
#' 
#' This function downloads a job from openEO if it is finished. You need to be
#' connected and logged in to run this function.
#' 
#' @param title Character string. The same title that has been used to start the
#' job by function [start_openEO_job()]. If ID is specified the title is still
#' used for the folder.
#' @param path The path where to download the data to
#' @param recentJob If TRUE, only the last 10 jobs are searched for the title 
#' @param ID If specified the Job ID is used instead of jobs title
#' @param overwrite If TRUE (Default is FALSE) existing folder will be used and 
#' previous data might be overwritten
#' 
#' @return 
#' Function only returns the information to which folder data is downloaded
#' 
#' @export
#' 
#' @importFrom openeo list_jobs download_results
#' 
download_openEO_job <- function(
    title, path, recentJob = TRUE, ID = NULL, overwrite = FALSE
){
  if(dir.exists(file.path(path, title)) & !overwrite){
    stop("Direcatory already exists and might contain data from previous jobs.")
  }
  
  allJobs <- openeo::list_jobs()
  if(recentJob){
    allJobs <- allJobs[1:10]
  }
  if(is.null(ID)){
    jobsFound <- which(sapply(allJobs, function(x){
      x$title == title
    }))
    if(length(jobsFound) == 0L){
      stop("No job called '", title, "' on openEO.")
    }
    if(length(jobsFound) > 1){
      stop("No unique job title. Please use ID instead")
    }
    j <- allJobs[[jobsFound]]
  } else {
    j <- allJobs[[ID]]
  }
  
  if(j$status == "finished"){
    openeo::download_results(
      job = j, 
      folder = file.path(path, title)
    ) 
  } else {
    print("Job is still running.")
    return(NULL)
  }
}
