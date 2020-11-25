#' @title Get Adobe Analytics Calculated Metrics List
#' 
#' @details Your integration must have admin access for this function to work.
#' 
#' @description Get a list of calculated metrics and their definitions.
#' 
#' 
#' @examples \dontrun{
#' calculated_metrics <- GetCalculatedMetrics()
#' }

GetCalculatedMetrics <- function(
  rsids = NULL,
  ownerId = NULL,
  filterByIds = NULL,
  toBeUsedinRsid = NULL,
  locale = "en_US",
  name = NULL,
  tagNames = NULL,
  favorite = FALSE,
  approved = FALSE,
  limit = 200,
  sortDirection = "ASC",
  sortProperty = "id",
  expansion = c("reportSuiteName", "ownerFullName", "modified"),
  includeType = c("all"),
  progress_messages = TRUE, 
  verbosity = FALSE
){
  
  initial_page <- 0
  calculated_metrics_df_list <- list()
  last_page_check = FALSE
  
  while(last_page_check == FALSE){
    url <- paste0(
      "https://analytics.adobe.io/api/",
      AdobeAnalytics$JWT_Credentials$company_id,
      "/calculatedmetrics?locale=",
      locale,
      ifelse(!is.null(rsids), paste0("&rsids=", paste(rsids, sep="", collapse = ",")), ''),
      ifelse(!is.null(ownerId), paste0("&ownerId=", ownerId), ''),
      ifelse(!is.null(filterByIds), paste0("&filterByIds=", paste(rsids, sep="", collapse = ",")), ''),
      ifelse(!is.null(toBeUsedinRsid), paste0("&toBeUsedInRsid=", toBeUsedinRsid), ''),
      ifelse(!is.null(name), paste0("&name=", name), ''),
      ifelse(!is.null(tagNames), paste0("&tagNames=", paste(tagNames, sep="", collapse = ",")), ''),
      ifelse(favorite == TRUE, "&favorite=true", ""),
      ifelse(approved == TRUE, "&approved=true", ""),
      paste0("&limit=", as.character(limit)),
      paste0("&page=", as.character(initial_page)),
      paste0("&sortDirection=", sortDirection),
      paste0("&sortProperty=", sortProperty),
      ifelse(!is.null(expansion), paste0("&expansion=", paste(expansion, sep="", collapse = ",")), ''),
      ifelse(!is.null(includeType), paste0("&includeType=", paste(includeType, sep="", collapse = ",")), '')
    )
    
    query_response <- JWTGet(
      url = url,
      accept_header = "application/json",
      content_type_header = "application/json",
      body_encoding = "json",
      verbose_output = verbosity
    )
    
    calculated_metrics_df <- query_response$content %>%
      mutate_if(is.list, function(x){rjson::toJSON(x)}) %>%
      mutate_if(is.numeric, function(x){as.character(x)}) %>%
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
        polarity,
        precision,
        type
      )
    
    
    calculated_metrics_df_list[[initial_page+1]] <- calculated_metrics_df
    
    if(progress_messages == TRUE){
      message(paste0("Pulled page ",
                     as.character(initial_page)))
    }
    
    
    initial_page <- initial_page + 1
    last_page_check <- query_response$lastPage
    
  }
  
  final_calculated_metrics_df <- dplyr::bind_rows(calculated_metrics_df_list)
  
  message(paste0("Returned info for ",
                 nrow(final_calculated_metrics_df),
                 " calculated metrics."
  ))
  
  final_calculated_metrics_df
  
}
