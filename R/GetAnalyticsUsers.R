#' @title Get Adobe Analytics User List
#' 
#' @details Your integration must have admin access for this function to work.
#' 
#' @description Get a list of all Adobe Analytics users in your organization.
#' 
#' @param limit
#' @param progress_messages
#' @param verbosity
#' 
#' @return Data frame
#' 
#' 
#' @export
#' 
#' @examples \dontrun{
#' analytics_users <- GetAnalyticsUsers()
#' }
#' 

GetAnalyticsUsers <- function(limit = 200,
                              progress_messages = TRUE, 
                              verbosity = FALSE){
  initial_page <- 0
  groups_df_list <- list()
  last_page_check = FALSE
  
  while(last_page_check == FALSE){
    url <- paste0("https://analytics.adobe.io/api/",
                  AdobeAnalytics$JWT_Credentials$company_id,
                  "/users?limit=",
                  as.character(limit),
                  "&page=",
                  as.character(initial_page))
    
    query_response <- JWTGet(url,
                             accept_header = "application/json",
                             content_type_header = "application/json",
                             body_encoding = "json",
                             verbose_output = verbosity)
    
    groups_df <- query_response$content
    groups_df_list[[initial_page+1]] <- groups_df
    
    if(progress_messages == TRUE){
      message(paste0("Pulled page ",
                     as.character(initial_page)))
    }
    
    
    initial_page <- initial_page + 1
    last_page_check <- query_response$lastPage
  }
  
  final_groups_df <- bind_rows(groups_df_list)
  message(paste0("Returned information for ",
                 nrow(final_groups_df),
                 " Analytics users in your organization."
  ))
  final_groups_df
}


