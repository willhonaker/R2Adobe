#' Delete Segments
#' Use 2.0 API to delete a segment
#' 
#' @param id id of segment to delete
#' @param verbosity verbose output in request.
#' 
#' @export
#' 
#' @keywords internal
#' 


DeleteSegment <- function(
  id,
  verbosity = FALSE
){
  result <- JWTDelete(
    "segments",
    "id",
    verbose_output = verbosity
  )
  
  if(result == "success"){
    message(
      paste0(
        "Successfully deleted segment: ",
        id
      )
    )
  } else {
    message(readable_response$result)  ## Catch-all for edge-case scenarios I may have missed.
    stop("Error: Unexpected result.  Please contact the package author with error details.")
  }
}








