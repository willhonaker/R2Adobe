#' @title Save Events
#' 
#' @details Enables/disables/updates events for a selection of RSIDs (bulk version coming soon).
#' 
#' @description Enables/disables/updates events for a selection of RSIDs (bulk version coming soon).
#' 
#' @param id
#' @param name 
#' @param description
#' @param rsids
#' @param type
#' @param default_metric
#' @param participation
#' @param serialization
#' 
#' @return Message indicating that the events were saved.
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @export
#' 
#' @examples \dontrun{
#' SaveEvents(id = "event10",
#'          name = "My Cool Event (e10)",
#'          type = "counter",
#'          description = "[Information about your Event here.]",
#'          rsids = c("myrisd1", "myrsid2", "myrsid3"))
#' }
#'
SaveEvents <- function(id,
                     name,
                     type,
                     description,
                     rsids,
                     default_metric = "true",
                     participation = "enabled",
                     serialization = "always_record",
                     verbosity = FALSE){

  ## Check enums here

  if(!(type %in% c("disabled", 
                   "counter_no_relations", 
                   "counter",
                   "currency_no_subrelations",
                   "currency",
                   "numeric_no_subrelations",
                   "numeric"))){
       stop("Invalid event type.")             
     }


  
  event_info <- list(id = id,
                    name = name,
                    type = type,
                    description = description,
                    default_metric = default_metric,
                    participation = participation,
                    serialization = serialization,
                    verbosity = FALSE)
  
  event_info_df <- data.frame(events = c(''))
  event_info_df$events <- list(data.frame(event_info))
  event_info_df$rsid_list <- list("the_rsid_list_goes_here")
  
  event_query <- toJSON(unbox(event_info_df), pretty=TRUE)
  event_query <- gsub("the_rsid_list_goes_here", paste(rsids, collapse = '","'), event_query)
  
  readable_response <- JWTPost("https://api.omniture.com/admin/1.4/rest/?method=ReportSuite.SaveEvents",
                       accept_header = "application/json",
                       content_type_header = "application/json",
                       body = event_query,
                       verbose_output = verbosity)
  if(readable_response == TRUE){
    message("Successfully saved event.")
  } else {
    message(readable_response) ## Is this possible?
    message("Event not updated! Check query and try again.")  
  }
}