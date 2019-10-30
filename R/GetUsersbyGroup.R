#' @title Get User Info by Group
#' @description Function to query list of users per specified group
#' @param groupstring user ID to pull info for
#' @param directOnly Something about whether membership is explicit or inherited
#' @param progress_messages
#' @param verbosity
#' 
#' @importFrom urltools url_encode
#' 
#' @return Data frame
#' 
#' @export
#' 
#' 
#' @examples \dontrun{
#' group_info <- GetUsersbyGroup("Some User Group")
#' }
#' 


GetUsersbyGroup <- function(groupstring,
                                 directOnly = FALSE,
                                 progress_messages = TRUE, 
                                 verbosity = FALSE){
  initial_page <- 0
  usersingroup_df_list <- list()
  last_page_check = FALSE
  
  while(last_page_check == FALSE){
    url <- paste0("https://usermanagement.adobe.io/v2/usermanagement/users/",
                  AdobeAnalytics$JWT_Credentials$org_id,
                  "/",
                  initial_page,
                  "/",
                  url_encode(groupstring))

    
    query_response <- JWTGet(url,
                             accept_header = "application/json",
                             content_type_header = "application/json",
                             body_encoding = "json",
                             body = paste0('{directOnly:',
                                           directOnly,
                                           '}'),
                             verbose_output = verbosity
                             )
    
    

    usersingroup_df <- query_response$users 
    usersingroup_df_list[[initial_page+1]] <- usersingroup_df
    
    if(progress_messages == TRUE){
      message(paste0("Pulled page ",
                     as.character(initial_page)))
    }
    
    
    initial_page <- initial_page + 1
    last_page_check <- query_response$lastPage
    
  }
  
  final_usersingroup_df <- bind_rows(usersingroup_df_list)
  message(paste0("Returned information for ",
                 nrow(final_usersingroup_df),
                 " users in group: ",
                 groupstring
  ))
  final_usersingroup_df
  
}





