##
##library(R2Adobe)


## one time frame + multiple metrics


TrendedDateReport <- function(
  rsid,
  start_date,
  end_date,
  dimension,
  metrics,
  segments = NULL,
  verbosity = FALSE,
  progress_messages = TRUE
){
  
  metric_json <- ''
  for(i in metrics){
    metric_json <- paste0(metric_json, '{"columnId":"',i,'", "id":"metrics/', i, '"},')
  }
  metric_json <- gsub(",$", "",metric_json)
  
  segment_json <- ''
  for(i in segments){
    segment_json <- paste0(segment_json, '{"type":"segment","segmentId":"',i,'"},')
  }
  segment_json <- gsub(",$", "",segment_json)
  
  ## Range goes here
  message(paste0("Querying data for ", rsid, " from ", start_date, " to ", end_date, ".")) ## better message later
  
  initial_page <- 0
  daily_data_list <- list()
  last_page_check = FALSE
  
  while(last_page_check == FALSE){
    query <- paste0('
        {
          "rsid": "',rsid,'",
          "globalFilters": [
              ',ifelse(!is.null(segments), paste0(segment_json, ","),''),'
              {
                  "type": "dateRange",
                  "dateRange": "',start_date,'T00:00:00.000/',end_date,'T00:00:00.000"
              }
          ],
          "metricContainer": {
              "metrics": [',metric_json,']
          },
          "dimension": "variables/',dimension,'",
          "settings": {
            "countRepeatInstances": true,
            "limit": 400,
            "page": ',initial_page,',
            "dimensionSort": "asc",
            "nonesBehavior": "return-nones"
          }
        }
      ')
    
    readable_response <- JWTRetry(
      "POST",
      url = paste0("https://analytics.adobe.io/api/", AdobeAnalytics$JWT_Credentials$company_id, "/reports"),
      body = query,
      body_encoding = "json",
      times = 30
    )
    
    response_df <- as.data.frame(readable_response$rows)
    ## account for empty response here
    
    data <- as.data.frame(do.call(rbind, response_df$data)) 
    data <- cbind(response_df$value, data) 
    names(data) <- c("date",readable_response$columns$columnIds)
    data$date <- strptime(data$date, format = "%b %d, %Y")
    daily_data_list[[initial_page+1]] <- data
    
    if(progress_messages == TRUE){
      message(paste0("Pulled page ",
                     as.character(initial_page)))
    }
    
    
    initial_page <- initial_page + 1
    last_page_check <- readable_response$lastPage
  }
  
  final_df <- bind_rows(daily_data_list)
  message(paste0("Successfully queried data for ", rsid))
  
  final_df
  
}
