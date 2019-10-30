#' @title Remove User from Organization
#' @description Function to remove users from your Adobe Organiztion.
#' @param userstring user to remove
#' @param confirm_delete Require user confirmation before deletion
#' @param verbosity
#' 
#' @return Data frame
#' 
#' @export
#' 
#' @examples \dontrun{
#' RemoveUserFromOrg("user@email.com")
#' }
#' 


RemoveUserFromOrg <- function(userstring,
                              confirm_delete = TRUE,
                              verbosity = FALSE){
  preexistence_check <- suppressMessages(GetUserInfo(userstring))
  if((preexistence_check$username) == "No matching records found."){
    stop("Specified username does not exist.")
  }
  
  
  ## Confirm deletion
  if(confirm_delete == TRUE){
    delete_choice <- menu(c("Yes", "No"), title = paste0("Are you sure you wish to remove user: ",
                                                          userstring,
                                                          "?"))
    if(delete_choice == 1){
      message("Beginning removal process...")
    } else if(delete_choice == 2){
      StopQuietly("Canceling removal.")
    }
  }
  
  
  ## Construct JSON query
  delete_info <- list(deleteAccount = FALSE)  ## This package will not support true account deletes for now
  query_df <- data.frame(user=c(userstring))
  
  delete_info_df <- data.frame(removeFromOrg = c(''))
  delete_info_df$removeFromOrg <- data.frame(delete_info)
  delete_info_df_nested <- data.frame(createEnterpriseID = delete_info_df)
  
  query_df$do <- list(list(unbox(delete_info_df_nested)))
  
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
    message(paste0("User ",
                   userstring,
                   " successfully removed from your organization."))  
  } else {
    message(readable_response$result)  ## Catch-all for edge-case scenarios I may have missed.
    stop("Error: Unexpected user action result.  Please contact the package author with error details.")
  }
  
}
