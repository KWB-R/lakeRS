#' Download results from a finished openEO job
#'
#' Searches for a finished openEO job by title or ID and downloads its results to
#' a local folder. The function assumes that the user is already connected and
#' authenticated with openEO.
#'
#' @param title Character scalar. Job title to search for and folder name used
#'   under `path`. If `ID` is supplied, `title` is still used as the local folder
#'   name.
#' @param path Character scalar. Directory into which the job folder is written.
#' @param recentJob Logical. If `TRUE`, only the ten most recent jobs are
#'   searched when `ID` is not supplied.
#' @param ID Optional openEO job identifier or list index. If supplied, this is
#'   used instead of title matching.
#' @param overwrite Logical. If `FALSE`, the function stops when the target
#'   directory already exists. Default is `FALSE`.
#'
#' @return Invisibly returns `NULL`. On success, job assets are written to
#'   `file.path(path, title)`. If the job is not finished, a message is printed
#'   and no files are downloaded.
#'
#' @importFrom openeo list_jobs download_results
#' @export
#' 
download_openEO_job <- function(
    title, path, recentJob = TRUE, ID = NULL, overwrite = FALSE
){
  if(dir.exists(file.path(path, title)) & !overwrite){
    stop("Directory already exists and might contain data from previous jobs.")
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
