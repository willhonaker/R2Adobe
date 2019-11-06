#' @title Realtime Report Query
#' @description Full functionality for 1.4 API Real-time endpoin.
#' 
#' @param rsid Target report suite for query.
#' @param date_from Start date for query period.  Accepts relative dates.
#' @param date_to End date for query period.  Accepts relative dates.
#' @param interval Interval in minutes by which report data is divided.
#' @param metric Target metric for report.
#' @param element Target element for report.  Optional.
#' @param top Number of top element values to display in report.  Defaults to 10.
#' @param classification Classification to use in place of specified element.
#' @param selected Comma-delimited list of element or classification values to match on.
#' @param search_keywords Keywords in result element values to match on.
#' @param search_type Possible options are "AND" and "OR".  Determines whether results must match to all keywords or at least one.
#' @param everything_else If TRUE, adds a row to results with metric sum for all other element values not contained in the result.
#' @param verbosity Toggle verbose output in request.
#' 
#' 
#' @return Data Frame
#' 
#' @export
#' 
#' @examples \dontrun{
#' mydata <- RawReportQuery(raw_query)
#' }
#' 

RealTimeReport <- function(rsid,
                           date_from = "1 hour ago",  ## All defaults should match API defaults.
                           date_to = "now",
                           interval = 5,
                           metric,
                           element = NULL,
                           top = 10,  
                           classification = NULL,
                           selected = NULL,
                           search_keywords = NULL,
                           search_type = "AND",
                           everything_else = FALSE, ## Does this match default in API?
                           verbosity = FALSE){
  
  query_info <- list(source = "realtime",
                     reportSuiteID = rsid,
                     dateFrom = date_from,
                     dateTo = date_to,
                     dateGranularity = paste0("minute:", interval), #minute should be the only valid unit
                     metrics = c(''))
  ## Metrics
  metric_info <- list(id = metric)
  
  ## Elements
  element_info <- list(id = element,
                       top = top,
                       everythingElse = everything_else)

  if(!is.null(classification)){
    element_info <- append(element_info, list(classification = classification))
  }
  
  if(!is.null(selected_list)){
    element_info <- append(element_info, list(selected = "SELECTEDLIST"))
  }
  
  if(!is.null(search_keywords)){
    element_info <- append(element_info, list(search = "SEARCHINFO"))
  }
  
    
  ## Construct Element selected and search keywords here
  if(!is.null(search_keywords)){
    if(search_type %in% c("AND", "OR")){
      search_string <- paste0('{"type":"', search_type,'","keywords": ["',  paste(search_keywords, collapse = '","'),'"]}')
    } else {
      stop("Invalid search type. Please use \"AND\" or \"OR\".")
    }
  }
  
  ## Construct JSON query
  query_df <- data.frame(reportDescription = c(''))
  query_df$reportDescription <- data.frame(query_info)
  query_df$reportDescription$metrics <- list(data.frame(metric_info))
  if(!is.null(element)){query_df$reportDescription$elements <- list(data.frame(element_info))}
  query_json <- toJSON(unbox(query_df), pretty=TRUE)
  query_json <- gsub('"SELECTEDLIST"', paste0('["', paste(selected_list, collapse = '","'), '"]'), query_json)
  query_json <- gsub('"SEARCHINFO"', search_string, query_json)
  
  ## Send POST request and parse data from response
  readable_response <- JWTPost(url,
                               accept_header = "application/json",
                               content_type_header = "application/json",
                               body = query_json,
                               verbose_output = verbosity)
  
  ###
  ## Add API error checking here
  ###
  
  response_data <- readable_response$report$data
  
  
  ## Parse response data into dataframe
  if(!is.null(element)){
    testlist <- lapply(1:nrow(response_data), function(z){
      breakdown_df <- as.data.frame(response_data$breakdown[z])
      binded_df <- cbind(response_data[z,][rep(1, nrow(df)), c(1:6,8)], breakdown_df)  
      names(binded_df)[1] <- c("date") 
      binded_df
    })
    final_df <- do.call(rbind, testlist)
    names(final_df)[names(final_df) == "name"] <- ifelse(!is.null(classification), classification, element)  
    names(final_df)[names(final_df) == "counts"] <- metric
    final_df <- final_df[,c(1:6, 8, 10, 9, 7)] ## more sensical column sorting
  } else {
    final_df <- response_data
    names(final_df)[names(final_df) == "name"] <- "date"
    names(final_df)[names(final_df) == "counts"] <- metric
  }
  final_df
}
