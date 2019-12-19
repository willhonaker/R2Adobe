#' @title Get JIL User Groups
#' 
#' @details 
#' 
#' @description Get a list of all user groups from JIL API.
#' 
#' @param token 
#' @param x_api_key
#' @param top
#' @param verbose_output Toggle verbose output in request.
#' 
#' 
#' @return Data frame 
#' 
#' @export
#' 
#' @examples \dontrun{
#' products <- JILProducts(token = token, x_api_key = x_api_key)
#' }
#' 
#' 

JILUserGroups <- function(token, 
                          x_api_key,
                          top = 1000,
                          verbosity = FALSE){
  url <- paste0("https://bps-il.adobe.io/jil-api/v2/organizations/", 
                AdobeAnalytics$JWT_Credentials$org_id, 
                "/user-groups?page_size=", 
                as.character(top))
  
  usergroup_data <- JILGet(url, 
                    token = token, 
                    x_api_key = x_api_key,
                    verbose_output = verbosity)
  
  usergroup_list <- list()
  for(i in 1:length(usergroup_data)){
    user_info <- usergroup_data[[i]]
    user_info <- as.data.frame(user_info, stringsAsFactors = FALSE)
    usergroup_list[[i]] <- user_info
  }
  
  usergroups <- bind_rows(usergroup_list)
  usergroups
}