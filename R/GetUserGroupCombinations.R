#' @title Get User Group Combinations
#' 
#' @details 
#' 
#' @description Helper function that gets you user by group data in long format.
#' 
#' @param progress_messages
#' @param verbosity
#' 
#' @return Data frame 
#' 
#' @export
#' 
#' @examples \dontrun{
#' users_by_group <- GetUserGroupCombinations()
#' }
#' 
#' 
#' 

GetUserGroupCombinations <- function(progress_messages = TRUE,
                                     verbosity = FALSE){
  
  message("Getting organization users.")
  org_users <- GetOrganizationUsers(progress_messages = progress_messages,
                                    verbosity = verbosity)
  
  message("Getting organization groups.")
  ui_user_groups <- GetOrganizationGroups(progress_messages = progress_messages,
                                          verbosity = verbosity)
  message("Assembling dataset.")
  
  
  
  org_users_list <- list()
  for(j in 1:nrow(org_users)){
    single_user <- org_users[j,]
    if(is.null(single_user$groups[[1]])){
      single_user$groups[[1]] = "no groups"
    }
    groups <- as.data.frame(single_user$groups[[1]], stringsAsFactors = FALSE)
    names(groups) <- "groups"
    for(i in names(single_user)[names(single_user) != "groups"]){
      groups[i] <- single_user[i]
    }
    groups <- groups[, c(2,1,3,4,5,6,7,8,9,10)]  ## beware of this hardcode
    org_users_list[[j]] <- groups
  }
  users_by_group <- do.call(rbind, org_users_list) %>%
                    left_join(ui_user_groups, 
                              by = c("groups" = "groupName")) %>%
                    rename(groupType = type.y,
                           accountType = type.x)
  message(paste0("Returned data for ", nrow(users_by_group), " user-group combinations in your organization."))
  users_by_group
  
}