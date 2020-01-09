#' @title Add User to Groups
#' 
#' @details Your integration must have admin access for this function to work.  
#' 
#' @description Adds a user to indicated product profiles/user groups.
#' 
#' @param username
#' @param groups
#' @param verbosity
#' 
#' @return Message indicating that the user was added to groups.
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @export
#' 
#' @examples \dontrun{
#' AddUserToGroups("user@mycompany.com", c("group1", "group2"))
#' }
#' 

AddUserToGroups <- function(username,
                            groups,
                            verbosity = FALSE){
  
  query_df <- data.frame(user=c(username))
  addinfo = list(group = groups)
  add_df <- data.frame(add = c(''))
  add_df$add <- list(addinfo)
  add_df_nested <- data.frame(add = add_df)
  query_df$do <- list(list(unbox(add_df_nested)))
  
  query_json <- toJSON(query_df, pretty=TRUE)
  
  readable_response <- JWTPost(url = paste0("https://usermanagement.adobe.io/v2/usermanagement/action/", 
                                            AdobeAnalytics$JWT_Credentials$org_id),
                               accept_header = "application/json",
                               content_type_header = "application/json",
                               body = query_json,
                               verbose_output = verbosity)
  
  if(readable_response$result == "error"){
    stop(paste0("Call was successful, but user action failed.\n",
                readable_response$errors$errorCode,
                ": ",
                readable_response$errors$message))
  } else if(readable_response$result == "success"){
    message(paste0(username, " added to following groups successfully:")) ## How to handle display of groups that it was added to.
    for(i in groups){
      message(i)
    }
  } else {
    message(readable_response$result)  ## Catch-all for edge-case scenarios I may have missed.
    stop("Error: Unexpected user action result.  Please contact the package author with error details.")
  }
}


