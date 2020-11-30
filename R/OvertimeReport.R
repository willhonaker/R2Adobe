## no dimensions except time

OvertimeReport <- function(
  rsid,
  start_date,
  end_date,
  granularity,
  metrics,
  segments = NULL,
  verbosity = FALSE,
  progress_messages = TRUE
){
  
  if(!(granularity %in% c(
    "day",
    "hour",
    "minute",
    "month",
    "year",
    "quarter",
    "week"
  ))){
    stop("Invalid time granularity specified.  Use one of following: day, hour, minute, month, year, quarter, week")
  }
  
  data <- TrendedReport(
    rsid = rsid,
    start_date = start_date,
    end_date = end_date,
    dimension = paste0('daterange', granularity),
    metrics = metrics,
    segments = segments,
    verbosity = verbosity,
    progress_messages = progress_messages
  )
  data
}