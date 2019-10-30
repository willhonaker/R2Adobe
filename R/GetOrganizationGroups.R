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
#' @export
#' 
#' @examples \dontrun{
#' organizations <- GetOrganizationGroups()
#' }
#' 


GetOrganizationGroups <- function(progress_messages = TRUE, 
                                  verbosity = FALSE){
  initial_page <- 0
  groups_df_list <- list()
  last_page_check = FALSE
  
  while(last_page_check == FALSE){
    url <- paste0("https://usermanagement.adobe.io/v2/usermanagement/groups/",
                  AdobeAnalytics$JWT_Credentials$org_id,
                  "/",
                  as.character(initial_page))
    
    query_response <- JWTGet(url,
                             accept_header = "application/json",
                             content_type_header = "application/json",
                             body_encoding = "json")
    
    ## Are there any errors specific to this kind of call not already handled by JWTGet?
    
    groups_df <- query_response$groups 
    
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
                 " groups in your organization."
  ))
  final_groups_df
}

