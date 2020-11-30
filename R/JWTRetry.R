JWTRetry <- function(
  verb = "POST",
  url,
  accept_header,
  content_type_header,
  body,
  body_encoding,
  times = 10,
  pause_base = 30,
  pause_cap = 60,
  pause_min = 1,
  handle = NULL,
  quiet = FALSE,
  terminate_on = c(400,401,403),
  terminate_on_success = TRUE
){
  
  report_request_raw <- RETRY(
    verb,
    url,
    config = (
      add_headers(Authorization = paste("Bearer", AdobeAnalytics$JWT_Credentials$access_token, sep = " "),
                  Accept = "application/json",
                  `Content-Type` = "application/json",
                  `x-proxy-global-company-id` = AdobeAnalytics$JWT_Credentials$company_id,
                  `x-api-key` = AdobeAnalytics$JWT_Credentials$api_key)
    ),
    body = body,
    encode = body_encoding,
    times = times,
    pause_base = pause_base,
    pause_cap = pause_cap,
    pause_min = pause_min,
    handle = handle,
    quiet = quiet,
    terminate_on = terminate_on,
    terminate_on_success = terminate_on_success
  )
  
  if(report_request_raw$status_code != 200){
    error_response <- content(report_request_raw, as = "text",type="application/json", encoding = "UTF-8")
    error_response <- fromJSON(error_response, flatten = TRUE)
    stop(paste0("Call failed with response code: ", 
                as.character(report_request_raw$status_code),
                ".\n",
                error_response$result,
                ": ",
                error_response$message))
  }
    
    readable_response <- jsonlite::fromJSON(content(report_request_raw, as = "text",type="application/json", encoding = "UTF-8"), flatten = TRUE)
    readable_response
  
}