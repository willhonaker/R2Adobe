#' @title Get Adobe Analytics Segments
#' 
#' @details 
#' 
#' @description Get a list of all Adobe Analytics segments.
#' 
#' @param include_type
#' @param rsids
#' @param segment_filters
#' @param locale
#' @param search_term
#' @param tags
#' @param expansion
#' @param limit
#' @param progress_messages
#' @param verbosity
#' 
#' @return Data frame 
#' 
#' @export
#' 
#' @examples \dontrun{
#' segments <- GetSegments()
#' }
#' 
#' 
#' 
GetSegments <- function(include_type = "all",
                        rsids = NULL,
                        segment_filters = NULL,
                        locale = "en_US",
                        search_term = NULL,
                        tags = NULL,
                        expansion = c("reportSuiteName",
                                      "ownerFullName",
                                      "modified",
                                      "tags"),  # definition and compatiblity possible but not included for now
                        limit = 1500,  ## Seems to be the max limit
                        progress_messages = TRUE, 
                        verbosity = FALSE
                        ){
  if(!(include_type %in% c("shared", "all", "templates", "deleted", "internal", "curatedItem"))){
    stop("Invalid argument for parameter: include_type")
  } ## For curated Item, need to also pass a "curated RSID"?
  
  if(!is.null(segment_filters) & !is.null(tags)){
    warning("Adding tag filters to a query using segment filters will return results matching either the segment filters or the tag filters.")
  } ## strangely the same doesn't happen for the combination of tags and search term
  
  
  ## Loop in here
  initial_page <- 0
  segments_df_list <- list()
  last_page_check = FALSE
  
  while(last_page_check == FALSE){
    url <- paste0("https://analytics.adobe.io/api/",
                  AdobeAnalytics$JWT_Credentials$company_id,
                  "/segments?includeType=",
                  include_type,  ## techincally in the API you can choose multiple options - worth it to change here?
                  "&locale=",
                  locale,
                  "&limit=",
                  limit,
                  "&page=",
                  initial_page)
    
    if(!is.null(rsids)){
      url <- paste0(url,
                    "&rsids=",
                    url_encode(paste(rsids, collapse = ",")))
    }
    
    ## segment filter
    
    if(!is.null(segment_filters)){
      url <- paste0(url,
                    "&segmentFilter=",
                    url_encode(paste(segment_filters, collapse = ",")))
    }
    
    ## name = can only be one value, I think?
    if(!is.null(search_term)){
      url <- paste0(url,
                    "&name=",
                    url_encode(search_term))
    }
    
    ## tags - will return all segments including at least one tag
    if(!is.null(tags)){
      url <- paste0(url,
                    "&tagNames=",
                    url_encode(paste(tags, collapse = ",")))
    }
    
    ## expansion
    if(!is.null(expansion)){
      url <- paste0(url,
                    "&expansion=",
                    url_encode(paste(expansion, collapse = ",")))
    }
    
    query_response <- JWTGet(url,
                             accept_header = "application/json",
                             content_type_header = "application/json",
                             body_encoding = "json",
                             verbose_output = verbosity)
    
    segments_df <- query_response$content

    segments_df_list[[initial_page+1]] <- segments_df
    
    if(progress_messages == TRUE){
      message(paste0("Pulled page ",
                     as.character(initial_page)))
    }
      
    initial_page <- initial_page + 1
    last_page_check <- query_response$lastPage
    }
    
    final_segments_df <- bind_rows(segments_df_list) %>%
      select(
        rsid,
        reportSuiteName,
        owner.id,
        owner.name,
        owner.login,
        id,
        name,
        description,
        modified,
        modifiedById
      )
    
    message(paste0("Returned information for ",
                   nrow(final_segments_df),
                   " segments."
    ))
    final_segments_df
}