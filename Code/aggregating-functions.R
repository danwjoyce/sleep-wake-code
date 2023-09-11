# helper functions to aggregate data in time series
require(zoo)
require(reshape2)
require(dplyr)
require(lubridate)

# DEPRECATED : difficult to explain why, but the cut(...) method used below
# appears to be failing as of 8th Dept 2023, returning lots of duplicated
# hours within a given day and it's not clear why

# SOLUTION : see new aggregateHours function below

# aggregateHours <- function( d ){
#   # takes a 'raw' participant's data (from readParticipantCSV) and
#   # bins it to hourly activity levels (d$SVM) and returns long-formatted data
#   
#   # zoo object version of SVM and time in hourly bins
#   x <- zoo( d$SVM, order.by = d$Time )
#   # try aggregating into 1 hour bins
#   
#   delta <- timeBin( d )
#   if( delta > 60 ) {
#     # then the data is already too course for hourly bins, so return an error
#     stop("aggregateHours: data is too coarse (greater than 60 minutes bins)")
#   } 
#   
#   agg.svm <- aggregate(x, cut(time(x), breaks="1 hours"), mean)
#   #new.long.df <- data.frame( Time = as.POSIXct( time( agg.svm ) ), Activity = as.numeric( agg.svm ) )
#   new.long.df <- data.frame( Time = as.POSIXct( time( agg.svm ), origin = "1970-01-01", tz = "GMT" ), Activity = as.numeric( agg.svm ) )
#   return( new.long.df )
# }

aggregateHours <- function( d ){
  
    # check in case data is too coarse grained to be aggregated by hours
    delta <- timeBin( d )
    if( delta > 60 ) {
      # then the data is already too course for hourly bins, so return an error
      stop("aggregateHours: data is too coarse (greater than 60 minutes bins)")
    }
    
    # 1 : add a grouping variable (implicitly casting d to a tibble) = floor_date() of the actual time stamp
    #     So, 12:30 becomes 12:00, 13:00 becomes 13:00, 13:30 becomes 13:00 etc...
    label.by.hour <- dplyr::group_by( dplyr::mutate( d, 
                                                     hour.label = lubridate::floor_date(Time, unit = "hour")
                                                     ), 
                                      hour.label )
    # 2 : use dplyr's summarise function, taking mean of SVM values, by the grouping variable hour.label
    label.by.hour <- dplyr::summarise( label.by.hour, SVM = mean(SVM, na.rm = TRUE)  )
    # 3 : return cast to a regular data.frame 
    new.long.df <- data.frame( Time = label.by.hour$hour.label, Activity = label.by.hour$SVM )
    
  
  return( new.long.df )
}
  

dayByHourMatrix <- function( df ) {
  # df -- output of aggregateHours
  # Returns: matrix with rows = Days and cols = Hours with elements = hourly activity summaries
  # Intended for periodogram analyses
  
  # add an integer hour and integer day index
  df$Hour <- as.numeric( format(strptime(df$Time, "%Y-%m-%d %H:%M:%S"),'%H') )
  df$Day <- cut( df$Time, breaks="1 days", labels=FALSE)
  
  # cast from long to wide format e.g. rows = Days, cols = Hours and tidy up row/col names
  m.df <- reshape2::dcast( df, Day ~ Hour, value.var = "Activity" )
  day.label <- m.df$Day
  m.df <- m.df[ , -which(names(m.df) %in% c("Day")) ]
  
  mat.df <- as.matrix( m.df, nrow = nrow(m.df), ncol = ncol(m.df) )
  rownames(mat.df) <- day.label
  return( mat.df )
}