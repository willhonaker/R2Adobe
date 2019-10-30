#' @title Get Organization Users
#' 
#' @details 
#' 
#' @description Get a list of all users in your organization.
#' 
#' @param progress_messages
#' @param verbosity
#' 
#' @return Data frame 
#' 
#' @export
#' 
#' @examples \dontrun{
#' users <- GetOrganizationUsers()
#' }
#' 


###

GetOrganizationUsers <- function(progress_messages = TRUE, 
                                 verbosity = FALSE){
  initial_page <- 0
  user_df_list <- list()
  last_page_check = FALSE
  
  while(last_page_check == FALSE){
    url <- paste0("https://usermanagement.adobe.io/v2/usermanagement/users/",
                  AdobeAnalytics$JWT_Credentials$org_id,
                  "/",
                  as.character(initial_page))
    
    query_response <- JWTGet(url,
                             accept_header = "application/json",
                             content_type_header = "application/json",
                             body_encoding = "json",
                             verbose_output = verbosity)
    
    ## Are there any errors specific to this kind of call not already handled by JWTGet?
    
    users_df <- query_response$users 
    
    user_df_list[[initial_page+1]] <- users_df
    
    if(progress_messages == TRUE){
      message(paste0("Pulled page ",
                     as.character(initial_page)))
    }


    initial_page <- initial_page + 1
    last_page_check <- query_response$lastPage
  }
  
  final_user_df <- bind_rows(user_df_list)
  message(paste0("Returned information for ",
                 nrow(final_user_df),
                 " users in your organization."
  ))
  final_user_df
}












