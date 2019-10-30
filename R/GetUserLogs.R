#' @title Get User Logs
#' @description Get user logs information for your login company.
#' @param start_date user to remove
#' @param end_date
#' @param verbosity
#' 
#' @return Data frame
#' 
#' @export
#' 
#' @examples \dontrun{
#' user_logs <- GetUserLogs(start_date = (Sys.Date()-30), end_date = Sys.Date())
#' }
#' 

GetUserLogs <- function(start_date,
                        end_date,
                        verbosity = FALSE){
  request_body <- toJSON(unbox(data.frame(date_from = start_date, date_to = end_date)), pretty=TRUE)
  response <- JWTPost("https://api.omniture.com/admin/1.4/rest/?method=Logs.GetUsageLog",
                      accept_header = "application/json",
                      content_type_header = "application/json",
                      body = request_body,
                      verbose_output = verbosity)
  response
}





