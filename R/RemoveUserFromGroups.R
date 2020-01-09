#' @title Remove User from Groups
#' 
#' @details Your integration must have admin access for this function to work.  
#' 
#' @description Removes a user from indicated product profiles/user groups.
#' 
#' @param username
#' @param groups
#' @param verbosity
#' 
#' @return Message indicating that the user was removed from groups.
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @export
#' 
#' @examples \dontrun{
#' RemoveUserFromGroups("user@mycompany.com", c("group1", "group2"))
#' }
#' 

RemoveUserFromGroups <- function(username,
                                 groups,
                                 verbosity = FALSE){
  
  query_df <- data.frame(user=c(username))
  if(groups == "all"){
    query_df$do <- list(data.frame(remove = "all"))
  } else {
    removeinfo = list(group = groups)
    remove_df <- data.frame(remove = c(''))
    remove_df$remove <- list(removeinfo)
    remove_df_nested <- data.frame(remove = remove_df)
    query_df$do <- list(list(unbox(remove_df_nested)))
  }
  
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
  } else if(readable_response$result == "success" & groups != "all"){
    message(paste0(username, " removed from following groups successfully:")) ## How to handle display of groups that it was added to.
    for(i in groups){
      message(i)
    }
  } else if(readable_response$result == "success" & groups == "all"){
    message(paste0(username, " removed from all groups successfully."))
  } else {
    message(readable_response$result)  ## Catch-all for edge-case scenarios I may have missed.
    stop("Error: Unexpected user action result.  Please contact the package author with error details.")
  }
  
}

