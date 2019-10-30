#' @title Create a User ID
#' 
#' @details Your integration must have admin access for this function to work.
#' 
#' @description Create a user ID for use with Adobe Experience Cloud.
#' 
#' @param username
#' @param firstname
#' @param lastname
#' @param country
#' @param add_groups
#' @param id_create_type
#' @param verbosity
#' @param if_exists
#' 
#' @return Message indicating that user was created
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @export
#' 
#' @examples \dontrun{
#' CreateUserID("user@website.com",
#' "Taro",
#' "Yamada",
#' "JP",
#' add_groups = c("Japan Users"))
#' }
#' 


CreateUserID <- function(username,
                       firstname,
                       lastname,
                       country,
                       add_groups = NULL,
                       id_create_type = "addAdobeID",
                       verbosity = FALSE,
                       if_exists = 'ignore'){
  
  ## Prevent user for inputting invalid JSON for "option" parameter.
  if(!(if_exists %in% c('ignore', 'update'))){
    stop("Invalid option for if_exists.\nYou must choose one of the following:\nignore\nupdate")
  } else if(if_exists == "ignore"){
    if_exists = "ignoreIfAlreadyExists"
  } else if(if_exists == "update"){
    if(id_create_type == "addAdobeID"){
      stop("Update actions not permitted on Adobe IDs.")
    } else {
      if_exists = "updateIfAlreadyExists"
    }
  }
  
  
  # In create ID case, check if Adobe ID exists in user's instance already.
  # Adobe doesn't do this check for us, so we do it here to avoid superfluous POST calls.
  # Otherwise the POST call would send and return 200, even though no real change was made.
  # If matching name does exist, will stop here, otherwise check returns an empty dataframe and code will continue.
  
  # TODO - check on validity of username inputs for "GetUserInfo".  04/25/19 - All good here.
  
  if(if_exists == 'ignoreIfAlreadyExists'){
    preexistence_check <- suppressMessages(GetUserInfo(username))
    if(tolower(username) == tolower(preexistence_check$username)){
      stop(paste0("Username ",
                  username,
                  " already exists for this instance."))
    }
  }
  
  ## Construct JSON body query.  Consider cleaning up this code a little.
  query_df <- data.frame(user=c(username))
  user_info <- list(email = username,
                   country = country,
                   firstname = firstname,
                   lastname = lastname,
                   option = if_exists
  )
  
  if(id_create_type == 'createEnterpriseID'){
    user_info_df <- data.frame(createEnterpriseID = c(''))
    user_info_df$createEnterpriseID <- data.frame(user_info)
    user_info_df_nested <- data.frame(createEnterpriseID = user_info_df)
  } else if(id_create_type == 'createFederatedID'){
    user_info_df <- data.frame(createFederatedID = c(''))
    user_info_df$createFederatedID <- data.frame(user_info)
    user_info_df_nested <- data.frame(createFederatedID = user_info_df)
  } else if(id_create_type == 'addAdobeID'){
    user_info_df <- data.frame(addAdobeID = c(''))
    user_info_df$addAdobeID <- data.frame(user_info)
    user_info_df_nested <- data.frame(addAdobeID = user_info_df)
  }
  
  
  if(is.null(add_groups)){
    query_df$do <- list(list(unbox(user_info_df_nested)))
  } else {
    addinfo = list(group = add_groups)
    add_df <- data.frame(add = c(''))
    add_df$add <- list(addinfo)
    add_df_nested <- data.frame(add = add_df)
    query_df$do <- list(list(unbox(user_info_df_nested), unbox(add_df_nested)))
  }

  
  query_json <- toJSON(query_df, pretty=TRUE)
  
  
  
  
  readable_response <- JWTPost(url = paste0("https://usermanagement.adobe.io/v2/usermanagement/action/", 
                     AdobeAnalytics$JWT_Credentials$org_id),
                     accept_header = "application/json",
                     content_type_header = "application/json",
                     body = query_json,
                     verbose_output = verbosity)
  
  # readable_response <- fromJSON(content(readable_response, as = "text", type="application/json"), flatten = TRUE) # NOT NEEDED, already in JWTPost
  ## Second check: 200 - but user was not created. Could happen if JSON inputs are not correct.
  ## Think of more sophisticated way of handling these?
  
  if(readable_response$result == "error"){
    stop(paste0("Call was successful, but user action failed.\n",
                readable_response$errors$errorCode,
                ": ",
                readable_response$errors$message))
  } else if(readable_response$result == "success"){
    message("User action successful.")
    if(if_exists == "ignoreIfAlreadyExists"){
      
      if(id_create_type == "addAdobeID"){
        message(paste0("Adobe ID created for: ",
                       username))
      } else if(id_create_type == "createEnterpriseID"){
        message(paste0("Enterprise ID created for: ",
                       username))
      } else if("createFederatedID"){
        message(paste0("Federated ID created for: ",
                       username))
      }
      
    } else {
      message(paste0("User metadata updated for: ",
                     username))
    }
  } else {
    message(readable_response$result)  ## Catch-all for edge-case scenarios I may have missed.
    stop("Error: Unexpected user action result.  Please contact the package author with error details.")
  }

}
          

  
  
  

