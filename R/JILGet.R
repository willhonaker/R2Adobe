#' JIL API GET Calls
#' Internal -  Make GET Calls to JIL API
#' 
#' @param url httr url for request
#' @param token 
#' @param x_api_key
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

JILGet <- function(url,
                   token,
                   x_api_key,
                   verbose_output = FALSE){
  if(verbose_output == TRUE){
    jil_request_raw <- GET(url,
                           add_headers(Authorization = paste0("Bearer ", token),
                                       Accept = "application/json, text/plain, */*",
                                       `X-Api-Key` = x_api_key),
                           verbose())
  } else {
    jil_request_raw <- GET(url,
                           add_headers(Authorization = paste0("Bearer ", token),
                                       Accept = "application/json, text/plain, */*",
                                       `X-Api-Key` = x_api_key))
  }
  
  if(jil_request_raw$status_code != 200){
    jil_response <- content(jil_request_raw)
    stop(paste0("GET Call failed with response code: ", 
                as.character(jil_request_raw$status_code),
                ".\n",
                jil_response$result,
                ": ",
                jil_response$message))
  } else {
    jil_response <- content(jil_request_raw)
  }
  jil_response
}

