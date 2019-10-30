#' @title Get User Info
#' @description Function to query single user info from User Management API
#' @param userstring user ID to pull info for
#' 
#' @importFrom urltools url_encode
#' 
#' @return Data frame
#' 
#' @export
#' 
#' 
#' @examples \dontrun{
#' GetUserInfo("user@email.com")
#' }
#' 


GetUserInfo <- function(userstring){
  query_response <- JWTGet(url = paste0("https://usermanagement.adobe.io/v2/usermanagement/organizations/", 
                           AdobeAnalytics$JWT_Credentials$org_id,
                           "/users/",
                           userstring
                           ),
                     accept_header = "application/json",
                     content_type_header = "application/json",
                     body_encoding = "json")
  
  if( !is.null(query_response$status_code) ){
    
    if(query_response$status_code == 404){
      user_df <- data.frame(username = 'No matching records found.')
      }
  
  } else {
    user_df <- query_response$user %>% 
      purrr::list_modify('groups' = NULL) %>%
      data.frame() %>%
      mutate(groups = list(query_response$user$groups))
    message(paste0("Successfully pulled info for:\n",
                   userstring))
  }

  user_df
}


## Testing



