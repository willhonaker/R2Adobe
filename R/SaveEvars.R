#' @title Save Evar
#' 
#' @details Enables/disables/updates an eVar for a selection of RSIDs (bulk version coming soon).
#' 
#' @description Enables/disables/updates an eVar for a selection of RSIDs (bulk version coming soon).
#' 
#' @param id
#' @param name
#' @param description
#' @param rsids
#' @param allocation_type
#' @param type
#' @param enabled
#' @param expiration_type
#' @param verbosity
#' 
#' @return Message indicating that the eVar was saved.
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @export
#' 
#' @examples \dontrun{
#' SaveEvars(id = "evar100",
#'          name = "My Cool eVar (v100)",
#'          description = "[Information about your eVar here.]",
#'          rsids = c("myrisd1", "myrsid2", "myrsid3"))
#' }
#'
SaveEvars <- function(id,
                     name,
                     description,
                     rsids,
                     allocation_type = "most_recent_last",
                     type = "0",
                     enabled = "true",
                     expiration_type = "0",
                     verbosity = FALSE){
  
  evar_info <- list(id = id,
                    name = name,
                    description = description,
                    allocation_type = allocation_type,
                    type = type,
                    enabled = enabled,
                    expiration_type = expiration_type)
  
  evar_info_df <- data.frame(evars = c(''))
  evar_info_df$evars <- list(data.frame(evar_info))
  evar_info_df$rsid_list <- list("the_rsid_list_goes_here")
  
  evar_query <- toJSON(unbox(evar_info_df), pretty=TRUE)
  evar_query <- gsub("the_rsid_list_goes_here", paste(rsids, collapse = '","'), evar_query)
  
  readable_response <- JWTPost("https://api.omniture.com/admin/1.4/rest/?method=ReportSuite.SaveEvars",
                       accept_header = "application/json",
                       content_type_header = "application/json",
                       body = evar_query,
                       verbose_output = verbosity)
  if(readable_response == TRUE){
    message("Successfully saved eVar.")
  } else {
    message(readable_response) ## Is this possible?
    message("eVar not updated! Check query and try again.")  
  }
}