#' @title Get Adobe Analytics Metrics
#' 
#' @details 
#' 
#' @description Get a list of all Adobe Analytics metrics in a given rsid.
#' 
#' @param rsid
#' @param locale
#' @param segmentable
#' @param expansion
#' @param verbosity
#' 
#' @return Data frame 
#' 
#' @export
#' 
#' @examples \dontrun{
#' metrics <- GetMetrics("my_rsid")
#' }
#' 


GetMetrics <- function(rsid,
                       locale = "en_US",
                       segmentable = FALSE,
                       expansion = NULL,
                       verbosity = FALSE){
  
  url <- paste0("https://analytics.adobe.io/api/",
                AdobeAnalytics$JWT_Credentials$company_id,
                "/metrics?rsid=",
                rsid,
                "&locale=",
                locale,
                "&segmentable=",
                segmentable)
  
  if(!(segmentable %in% c(TRUE, FALSE))){
    stop("Invalid argument for parameter: segmentable")
    
  }
  
  if(!is.null(expansion)){  ## Make this so it can handle multiple metadata tags - Add later if you see a case where this is true
    
    if(!(expansion %in% c("tags"))){ ## expand outward if Adobe makes changes to the API
      stop("Invalid argument for parameter: expansion")
    } else {
      metadata = expansion
      url <- paste0(url, "&expansion=", expansion)
    }

  }
  
  
  query_response <- JWTGet(url,
                           accept_header = "application/json",
                           content_type_header = "application/json",
                           body_encoding = "json",
                           verbose_output = verbosity)
  query_response
}




