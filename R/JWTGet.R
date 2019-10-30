#' JWT API GET Calls
#' Internal -  Make GET Calls to 2.0 API
#' 
#' @param url httr url for request
#' @param accept_header Accept header for request
#' @param content_type_header Content type header for request
#' @param body Request body
#' @param body_encoding httr parameter for body encoding.  Choices should be "form", "json", "raw"
#' @param verbose_output Toggle verbose output in request.
#' 
#' @importFrom httr add_headers GET
#' 
#' @return raw response
#' 
#' @export
#' 
#' @keywords internal
#' 


JWTGet <- function(url,
                    accept_header,
                    content_type_header,
                    body = NULL,
                    body_encoding = "json",
                    verbose_output = FALSE
){
  
  if(verbose_output == TRUE){
    report_request_raw <- GET(url,
                               add_headers(Authorization = paste("Bearer", AdobeAnalytics$JWT_Credentials$access_token, sep = " "),
                                           Accept = accept_header,
                                           `Content-Type` = content_type_header,
                                           `x-proxy-global-company-id` = AdobeAnalytics$JWT_Credentials$company_id,
                                           `x-api-key` = AdobeAnalytics$JWT_Credentials$api_key),
                               body = body,
                               encode = body_encoding,
                               verbose())
  } else {
    report_request_raw <- GET(url,
                               add_headers(Authorization = paste("Bearer", AdobeAnalytics$JWT_Credentials$access_token, sep = " "),
                                           Accept = accept_header,
                                           `Content-Type` = content_type_header,
                                           `x-proxy-global-company-id` = AdobeAnalytics$JWT_Credentials$company_id,
                                           `x-api-key` = AdobeAnalytics$JWT_Credentials$api_key),
                               body = body,
                               encode = body_encoding) 
  }
  
  
  #### Think about how to handle a 429 error here.
  
  if(report_request_raw$status_code == 404){
    message("404: No matching records found.")
    readable_response <- report_request_raw
  } else if(report_request_raw$status_code != 200){
    error_response <- content(report_request_raw, as = "text",type="application/json", encoding = "UTF-8")
    error_response <- fromJSON(error_response, flatten = TRUE)
    stop(paste0("GET Call failed with response code: ", 
                as.character(report_request_raw$status_code),
                ".\n",
                error_response$result,
                ": ",
                error_response$message))
  } else {
    readable_response <- fromJSON(content(report_request_raw, 
                                          as = "text",
                                          type="application/json", 
                                          encoding = "UTF-8"), 
                                  flatten = TRUE)
  }
  readable_response
}





