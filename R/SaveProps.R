#' @title Save Prop
#' 
#' @details Enables/disables/updates a prop for a selection of RSIDs (bulk version coming soon).
#' 
#' @description Enables/disables/updates a prop for a selection of RSIDs (bulk version coming soon).
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
#' @return Message indicating that the prop was saved.
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @export
#' 
#' @examples \dontrun{
#' SaveProps(id = "prop10",
#'          name = "My Cool Prop (c10)",
#'          description = "[Information about your prop here.]",
#'          rsids = c("myrisd1", "myrsid2", "myrsid3"))
#' }
#'
SaveProps <- function(id,
                     name,
                     description,
                     rsids,
                     enabled = "true",
                     pathing_enabled = "true",
                     list_enabled = "false",
                     participation_enabled = "false",
                     verbosity = FALSE){
  
  prop_info <- list(id = id,
                    name = name,
                    description = description,
                    enabled = enabled,
                    pathing_enabled = pathing_enabled,
                    list_enabled = list_enabled,
                    participation_enabled = participation_enabled,
                    verbosity = FALSE)
  
  prop_info_df <- data.frame(props = c(''))
  prop_info_df$props <- list(data.frame(prop_info))
  prop_info_df$rsid_list <- list("the_rsid_list_goes_here")
  
  prop_query <- toJSON(unbox(prop_info_df), pretty=TRUE)
  prop_query <- gsub("the_rsid_list_goes_here", paste(rsids, collapse = '","'), prop_query)
  
  readable_response <- JWTPost("https://api.omniture.com/admin/1.4/rest/?method=ReportSuite.SaveProps",
                       accept_header = "application/json",
                       content_type_header = "application/json",
                       body = prop_query,
                       verbose_output = verbosity)
  if(readable_response == TRUE){
    message("Successfully saved prop.")
  } else {
    message(readable_response) ## Is this possible?
    message("Prop not updated! Check query and try again.")  
  }
}