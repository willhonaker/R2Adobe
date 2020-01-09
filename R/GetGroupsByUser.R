#' @title Get Groups By User
#' @description Get list of groups currently assigned to a user.
#' @param groupstring user ID to pull info for
#' @param verbosity
#' 
#' 
#' @return Data frame
#' 
#' @export
#' 
#' 
#' @examples \dontrun{
#' my_groups <- GetGroupsByUser("myuser@mycompany.com")
#' }
#' 


GetGroupsByUser <- function(username, verbosity = FALSE){
  userinfo <- suppressMessages(GetUserInfo(username, verbosity = verbosity))
  groups <- as.data.frame(userinfo$groups)  
  names(groups) <- "groups"
  groups$username <- username
  groups <- groups[,c("username", "groups")]
  message(paste0("Pulled group list successfully. ", 
          username, 
          " currently belongs to ", 
          as.character(nrow(groups)), " groups."))
  groups
}




