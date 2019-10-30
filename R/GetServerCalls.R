#' @title Get Server Calls
#' @description Get Server Call Usage information for your Adobe Organization.
#' @param start_date user to remove
#' @param end_date
#' @param date_granularity
#' @param endpoint_number
#' @param verbosity
#' 
#' @return Data frame
#' 
#' @export
#' 
#' @examples \dontrun{
#' server_calls <- GetServerCalls(start_date = (Sys.Date()-30), end_date = Sys.Date())
#' }
#' 

GetServerCalls <- function(start_date,
                           end_date,
                           date_granularity = "day",
                           endpoint_number = "5-4",
                           verbosity = FALSE){
  
  if(date_granularity == "year"){
    vars_to_agg <- c("year")
  } else if(date_granularity == "month"){
    vars_to_agg <- c("year", "month")
  } else if(date_granularity == "week"){
    vars_to_agg <- c("year", "week")
  } else if(date_granularity == "day"){
    vars_to_agg <- c("day")
  } else {
    stop("Invalid date input.")
  }
  
  days <- as.character(seq.Date(from = as.Date(start_date),
                                to = as.Date(end_date),
                                by = "day"
  ))
  
  group_by_columns <- function(dataframe, group_columns){
    group_by(dataframe, !!!syms(group_columns))
  }
  
  usage_list <- list()
  
  for(i in 1:length(days)){
    print(paste0("beginning pull for ", days[i]))
    url = paste0("https://appservice", endpoint_number,".omniture.com/analytics/1.0/servercallusage/servercalls/reportsuites?locale=en_US&expansions=reportSuiteName&start_date=", 
                 days[i], 
                 "&end_date=", 
                 days[i],
                 "&pagination=true&page=0&limit=1000")
    
    readable_response <- JWTGet(url,
                                accept_header = "application/json",
                                content_type_header = "application/json",
                                body_encoding = "json",
                                verbose_output = verbosity)
    usage_df <- as.data.frame(readable_response$rows)
    usage_df$day <- days[i]
    usage_list[[i]] <- usage_df
  }
  
  final_df <- do.call(rbind, usage_list)
  
  metrics_df <- as.data.frame(do.call(rbind, final_df$usages))
  names(metrics_df) <- c("Primary_Calls",
                         "Secondary_Calls",
                         "Mobile_Primary_Calls",
                         "Mobile_Secondary_Calls",
                         "Total_Calls")
  
  final_df <- cbind(final_df[,c("day",
                                "rsid")], 
                    metrics_df)
  
  final_df <- final_df %>% 
    mutate(Primary_Calls = as.numeric(Primary_Calls) + as.numeric(Mobile_Primary_Calls),
           Secondary_Calls = as.numeric(Secondary_Calls) + as.numeric(Mobile_Secondary_Calls),
           year = year(as.Date(day)),
           month = month(as.Date(day)),
           week = week(as.Date(day)),
           day_number = day(as.Date(day))) %>%
    select(day,
           year,
           month,
           week,
           day_number,
           rsid,
           Primary_Calls,
           Secondary_Calls,
           Total_Calls)
  
  ## just the symbols in the group by statement
  
  if(date_granularity == "year"){
    vars_to_agg <- c("year")
  } else if(date_granularity == "month"){
    vars_to_agg <- c("year", "month")
  } else if(date_granularity == "week"){
    vars_to_agg <- c("year", "week")
  } else if(date_granularity == "day"){
    vars_to_agg <- c("day", "year", "month", "week", "day_number")
  } else {
    stop("Invalid date input.")
  }
  
  vars_to_agg <- c("rsid", vars_to_agg)
  
  final_df_agg <- final_df %>%
    group_by_columns(vars_to_agg) %>%
    dplyr::summarize(Primary_Calls = sum(as.numeric(Primary_Calls)),
                     Secondary_Calls = sum(as.numeric(Secondary_Calls)),
                     Total_Calls = Primary_Calls + Secondary_Calls) 
 final_df_agg
}




