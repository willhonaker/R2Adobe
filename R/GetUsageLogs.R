#' @title Get Usage Logs
#' @description Function to query 2.0 Usage APIs



GetUsageLogs <- function(
  start_date,
  end_date,
  limit = 200,
  progress_messages = TRUE, 
  verbosity = FALSE
){
  initial_page <- 0
  usage_logs_list <- list()
  last_page_check = FALSE
  
  while(last_page_check == FALSE){
    url <- paste0(
      "https://analytics.adobe.io/api/",
      AdobeAnalytics$JWT_Credentials$company_id,
      "/auditlogs/usage?startDate=",
      as.character(start_date),
      "T00%3A00%3A00-07",
      "&endDate=",
      as.character(end_date),
      "T23%3A59%3A59-07",
      "&limit=",
      as.character(limit),
      "&page=",
      as.character(initial_page)
    )
    
    query_response <- JWTGet(url,
                             accept_header = "application/json",
                             content_type_header = "application/json",
                             body_encoding = "json",
                             verbose_output = verbosity)
    
    logs_df <- query_response$content
    usage_logs_list[[initial_page+1]] <- logs_df
    
    if(progress_messages == TRUE){
      message(paste0("Pulled page ",
                     as.character(initial_page)))
    }
    
    
    initial_page <- initial_page + 1
    last_page_check <- query_response$lastPage
  }
  
  usage_groups_df <- bind_rows(usage_logs_list)
  message(paste0("Returned information for ",
                 nrow(usage_groups_df),
                 "logged analytics actions in the specified time range."
  ))
  usage_groups_df
}







