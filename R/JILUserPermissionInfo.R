#' @title Get all permission items and their parent profiles/groups for a single user.
#' 
#' @details 
#' 
#' @description Get a list of all user groups from JIL API.
#' 
#' @param username
#' @param product_id
#' @param token 
#' @param x_api_key
#' 
#' @return Data frame 
#' 
#' @export
#' 
#' @examples \dontrun{
#' permission_detail <- JILUserPermissionInfo(
#' username = "example@useremail.com"
#' token = token, 
#' x_api_key = x_api_key
#' )
#' }
#' 
#' 
#' 
#' 
JILUserPermissionInfo <- function(username, 
                                  product_id,
                                  token,
                                  x_api_key){  
  
  message("Getting license group information...")
  license_groups <- JILLicenseGroups(item_id = product_id,
                                     token = token, 
                                     x_api_key = x_api_key)
  
  message("Getting user group information...")
  jil_user_groups <- JILUserGroups(token = token,
                                   x_api_key = x_api_key)
  ui_user_groups <- GetOrganizationGroups()
  
  
  single_user <- suppressMessages(GetUserInfo(username))
  if(single_user$username == "No matching records found."){
    stop("404: No matching user records found.")
  } else if(single_user$username != single_user$username){
    stop("Could not pull user info.")
  }
  message(paste0("Querying user permissions for: ", username))
  
  
  if(is.null(single_user$groups[[1]])){
    single_user$groups[[1]] = "no groups"
  }
  
  if(is.null(single_user$adminRoles)){
    single_user$adminRoles = NA
  }
  
  groups <- as.data.frame(single_user$groups, stringsAsFactors = FALSE)
  names(groups) <- "groups"
  for(i in names(single_user)[names(single_user) != "groups"]){
    groups[i] <- single_user[i]
  }

  groups <- groups[,c("username",
                      "email",
                      "status",
                      "adminRoles",
                      "domain",
                      "firstname",
                      "lastname",
                      "country",
                      "type",
                      "groups")]

  ## Product Profiles
  groups <- groups %>% 
            left_join(ui_user_groups, 
                      by = c("groups" = "groupName")) %>%
            rename(groupType = type.y,
                   accountType = type.x)
  
  if(nrow(groups[groups$groupType == "USER_GROUP",]) == 0){
    groups <- groups[groups$groupType == "PRODUCT_PROFILE",]
    groups$inheritance <- "Direct Assignment"
    groups$inheritance_id <- NA
  } else {
    user_groups <- groups[groups$groupType == "USER_GROUP",]
    user_groups_list <- lapply(unique(user_groups$groups), function(x){
      id <- jil_user_groups$id[jil_user_groups$name == x]
      usergroup_df <- JILLicenseGroups(item_id = id, 
                                       get_by = "usergroup",
                                       jil_token, 
                                       x_api_key)
      if(is.na(usergroup_df$id[1])){  ## set df to NULL if there are no license groups associated
        usergroup_df <- NULL
      } else {
        usergroup_df$inheritance <- x
        usergroup_df$inheritance_id <- id
      }
      usergroup_df
    })
    
    user_groups <- bind_rows(user_groups_list) %>%
      mutate(username = single_user$username,
             firstname = single_user$firstname,
             lastname = single_user$lastname,
             email = single_user$email,
             domain = single_user$domain,
             status = single_user$status,
             adminRoles = single_user$adminRoles,
             country = single_user$country,
             accountType = single_user$type,
             groupType = "PRODUCT_PROFILE") %>%
      rename(groups = name,
             productName = product.longName) %>%
      select(username,
             firstname,
             lastname,
             email,
             domain,
             status,
             adminRoles,
             country,
             accountType,
             groups,
             groupType,
             productName,
             inheritance,
             inheritance_id)
    
    groups <- groups[groups$groupType == "PRODUCT_PROFILE",]
    groups$inheritance <- "Direct Assignment"
    groups$inheritance_id <- NA
    groups <- bind_rows(groups, user_groups)
  }
  

  permissions_df_list <- list()
  for(k in 1:length(groups$groups)){
    id <- license_groups$id[license_groups$name == groups$groups[k]]
    if(length(id) == 0){
      permissions_df <- NULL
    } else {
      message(paste0("Pulling permissions for: ", groups$groups[k]))
      permissions_df <- JILPermissions(licensegroup_id = id,
                                       product_id = product_id,
                                       token = jil_token,
                                       x_api_key = x_api_key)
      permissions_df <- permissions_df %>%
        mutate(username = single_user$username,
               firstname = single_user$firstname,
               lastname = single_user$lastname,
               email = single_user$email,
               domain = single_user$domain,
               status = single_user$status,
               admin_roles = single_user$adminRoles,
               country = single_user$country,
               type = single_user$type,
               license_group = groups$groups[k],
               inheritance = groups$inheritance[k],
               inheritance_id = groups$inheritance_id[k]) %>%
        select(username,
               firstname,
               lastname,
               email,
               domain,
               status,
               admin_roles,
               country,
               type,
               license_group,
               inheritance,
               inheritance_id,
               permissions_type,
               permissions_item,
               permissions_name,
               included) %>%
        mutate_if(is.factor, as.character) %>%
        filter(included == TRUE) 
    }

    permissions_df_list[[k]] <- permissions_df
  }
  permissions_df_final <- bind_rows(permissions_df_list)
  message(paste0("Pulled detailed permission info for ", length(unique(permissions_df_final$license_group)), " groups."))
  permissions_df_final
}