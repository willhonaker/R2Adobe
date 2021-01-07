#' JWT API DELETE Calls
#' Internal -  Make DELETE Calls to 2.0 API
#' 
#' @param url httr url for request
#' @param accept_header Accept header for request
#' @param content_type_header Content type header for request
#' @param verbose_output Toggle verbose output in request.
#' @param method
#' 
#' @importFrom httr add_headers GET
#' 
#' @return raw response
#' 
#' @export
#' 
#' @keywords internal
#' 



JWTDelete <- function(
  method,
  id,
  accept_header = "application/json",
  content_type_header = "application/json",
  verbose_output
){
  
  if(!(method %in% c(
    "segments",
    "calculatedmetrics"
  ))){
    stop("Invalid method.  Specify eiher 'segments' or 'calculatedmetrics'.")
  }
  
  url <- paste0(
    "https://analytics.adobe.io/api/",
    AdobeAnalytics$JWT_Credentials$company_id,
    "/",
    method,
    "/",
    id
  )
  
  if(verbose_output == TRUE){
    report_request_raw <- DELETE(url,
                              add_headers(Authorization = paste("Bearer", AdobeAnalytics$JWT_Credentials$access_token, sep = " "),
                                          Accept = accept_header,
                                          `Content-Type` = content_type_header,
                                          `x-proxy-global-company-id` = AdobeAnalytics$JWT_Credentials$company_id,
                                          `x-api-key` = AdobeAnalytics$JWT_Credentials$api_key),
                              verbose())
  } else {
    report_request_raw <- DELETE(url,
                              add_headers(Authorization = paste("Bearer", AdobeAnalytics$JWT_Credentials$access_token, sep = " "),
                                          Accept = accept_header,
                                          `Content-Type` = content_type_header,
                                          `x-proxy-global-company-id` = AdobeAnalytics$JWT_Credentials$company_id,
                                          `x-api-key` = AdobeAnalytics$JWT_Credentials$api_key)) 
  }
  
  if(report_request_raw$status_code == 404){
    stop("404: Item not found.")
  } else if(report_request_raw$status_code != 200){
    error_response <- content(report_request_raw, as = "text",type="application/json", encoding = "UTF-8")
    error_response <- fromJSON(error_response, flatten = TRUE)
    stop(paste0("DELETE Call failed with response code: ", 
                as.character(report_request_raw$status_code),
                ".\nError Code: ",
                error_response$errorCode,
                "\nError Description: ",
                error_response$errorDescription,
                "\nError ID: ",
                error_response$errorId
                ))
  } else {
    readable_response <- fromJSON(content(report_request_raw, 
                                          as = "text",
                                          type="application/json", 
                                          encoding = "UTF-8"), 
                                  flatten = TRUE)
  }
  readable_response$result
}