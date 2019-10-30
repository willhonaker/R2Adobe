#' @title Get Organization Groups
#' 
#' @details 
#' 
#' @description Get a list of all user groups.
#' 
#' @param progress_messages
#' @param verbosity
#' 
#' @return Data frame 
#' 
#' @importFrom urltools url_encode
#' 
#' @export
#' 
#' @examples \dontrun{
#' rsids <- GetReportSuites()
#' }
#' 


GetReportSuites <- function(rsids = NULL,
                            search_term = NULL,  ## searches in the rsid field
                            expansion = c("name", ## friendly name
                                          "parentRsid",  # for VRS
                                          "currency",
                                          "calendarType",
                                          "timezoneZoneinfo"),
                            limit = 1500,
                            progress_messages = TRUE, 
                            verbosity = FALSE){
  initial_page <- 0
  rsids_df_list <- list()
  last_page_check = FALSE
  
  
  while(last_page_check == FALSE){
    
    url <- paste0("https://analytics.adobe.io/api/",
                  AdobeAnalytics$JWT_Credentials$company_id,
                  "/collections/suites?limit=",
                  limit,
                  "&page=",
                  initial_page)
    
    
    if(!is.null(rsids)){
      url <- paste0(url,
                    "&rsids=",
                    url_encode(paste(rsids, collapse = ",")))  ## space doesn't work here
    }
    
    if(!is.null(search_term)){
      url <- paste0(url,
                    "&rsidContains=",
                    url_encode(search_term))
    }
    
    ## expansion
    if(!is.null(expansion)){
      url <- paste0(url,
                    "&expansion=",
                    url_encode(paste(expansion, collapse = ",")))
    }
    
    query_response <- JWTGet(url,
                             accept_header = "application/json",
                             content_type_header = "application/json",
                             body_encoding = "json",
                             verbose_output = verbosity)
    
    rsids_df <- query_response$content
    
    rsids_df_list[[initial_page+1]] <- rsids_df
    
    if(progress_messages == TRUE){
      message(paste0("Pulled page ",
                     as.character(initial_page)))
    }
    
    initial_page <- initial_page + 1
    last_page_check <- query_response$lastPage
  }
  
  
  final_rsids_df <- bind_rows(rsids_df_list)
  message(paste0("Returned information for ",
                 nrow(final_rsids_df),
                 " report suites."
  ))
  final_rsids_df
}