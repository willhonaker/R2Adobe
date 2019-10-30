#' @title Raw Report Query
#' @description Query the 2.0 API reports endpoint.
#' 
#' @param raw_query A JSON request query.
#' @param verbosity Toggle verbose output in request.
#' 
#' 
#' @return Data Frame
#' 
#' @export
#' 
#' @examples \dontrun{
#' mydata <- RawReportQuery(raw_query)
#' }
#' 


RawReportQuery <- function(raw_query, verbosity = FALSE){
  
  readable_response <- JWTPost(url = paste0("https://analytics.adobe.io/api/",
                               AdobeAnalytics$JWT_Credentials$company_id,
                               "/reports"),
                               accept_header = "application/json",
                               content_type_header = "application/json",
                               body = raw_query,
                               verbose_output = verbosity)
  
  response_df <- as.data.frame(readable_response$rows)
  data <- as.data.frame(do.call(rbind, response_df$data)) 
  data <- cbind(response_df$value, data) 
  names(data) <- c(readable_response$columns$dimension$id, readable_response$columns$columnIds)

  data
  
}



