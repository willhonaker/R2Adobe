#' Delete Calculated Metrics
#' Use 2.0 API to delete a calculated metric
#' 
#' @param id id of calculated metric to delete
#' @param verbosity verbose output in request.
#' 
#' @export
#' 
#' @keywords internal
#' 


DeleteCalculatedMetric <- function(
  id,
  verbosity = FALSE
){
  result <- JWTDelete(
    "calculatedmetrics",
    id,
    verbose_output = verbosity
  )
  
  if(result == "success"){
    message(
      paste0(
        "Successfully deleted calculated metric: ",
        id
      )
    )
  } else {
    message(readable_response$result)  ## Catch-all for edge-case scenarios I may have missed.
    stop("Error: Unexpected result.  Please contact the package author with error details.")
  }
}

