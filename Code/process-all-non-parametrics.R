source("setup-env.R")
source("read-participant.R")
source("aggregating-functions.R")
source("periodogram-functions.R")
source("visualisation-functions.R")
source("non-parametric-measures.R")


file.list <- list.files(data.path)

# remove existing output from previous runs of this script
unlink(paste0( output.path, "/*"))

df <- data.frame(
  id = seq(1, length(file.list), by = 1 ),
  processing.timestamp = rep( as.POSIXct( NA ), length( file.list) ),
  fname = file.list,
  process.QC = rep("", length( file.list ) ),
  IS = rep(NA, length( file.list ) ),
  IV = rep(NA, length( file.list ) ),
  M10.hour = rep(NA, length( file.list ) ),
  M10.value = rep(NA, length( file.list ) ),
  L5.hour = rep(NA, length( file.list ) ),
  L5.value = rep(NA, length( file.list ) )
)

for( i in 1:length( file.list) ) {

  this.error.flag <- FALSE
  
  # 1. load data from CSV file, just as for periodogram
  this.sub <- file.list[ i ]
  d <- readParticipantCSV( data.path, this.sub )

  # 2. Aggreagte and convert to [day,hour] matrix format
  mat.df <- try({
    dayByHourMatrix( aggregateHours( d ) )
  }, silent = TRUE)
  
  # 3 plot activity
  act.plot <- try({
    activityHeatmap( mat.df, this.sub )
  }, silent = TRUE)
  
  # 4 check that the SVM is > 0 for all time points
  # -- Reason: SVM is gravity-corrected acceleration e.g. sqrt( x^2 + y^2 + z^2 ) - 1
  # -- Some participants have such small magnitude (x,y,z) values that the SVM is < 0 so
  #    See : https://github.com/danwjoyce/accel-scripts/blob/master/Code/cwa2svm.py

  if( 
      ( class( act.plot )[1] == "try-error" ) |
      ( sum( d$SVM ) == 0 ) 
    ) {
    # attempt to pre-process has failed - usually, this error is caught because
    # < 24 hours of data or all SVM data is zero
    print( paste0( "process-all-non-parametrics.R : filename ", this.sub, " is either corrupted, does not contain enough data (< 24 hours) or SVM data is effectively zero" ) )
    print( paste0( " -- The source data contains ", nrow( d ), " rows " ) )
    this.error.flag <- TRUE
  } else {
  
    ggsave( filename = paste0( file.path( output.path, file.list[i] ), ".png" )
            , bg = "white" )
    
    # 3. Compute summary statistics
    df$IS[ i ]  <- interdailyStability( mat.df )
    df$IV[ i ]  <- intradailyVariability( mat.df )
    
    this.M10 <- try({
      compute_M10( mat.df )
    }, silent = TRUE)
    
    if( class( this.M10 )[1] == "try-error" ) {
      print( paste0( "process-all-non-parametrics.R : compute_M10 failed on filename ", this.sub, " -- usually, this means too few usable data points" ) )
      this.error.flag <- TRUE
      df$M10.hour[ i ]   <- NA
      df$M10.value[ i ]  <- NA
    } else {
      df$M10.hour[ i ]  <- this.M10$M10.hour  
      df$M10.value[ i ] <- this.M10$M10.mean.activity
    }
    
    this.L5  <- try({
      compute_L5( mat.df )
    }, silent = TRUE)

    if( class( this.L5 )[1] == "try-error" ) {
      print( paste0( "process-all-non-parametrics.R : compute_L5 failed on filename ", this.sub, " -- usually, this means too few usable data points" ) )
      this.error.flag <- TRUE
      df$L5.hour[ i ]    <- NA 
      df$L5.value[ i ]   <- NA
    } else {
      df$L5.hour[ i ]  <- this.L5$L5.hour
      df$L5.value[ i ] <- this.L5$L5.mean.activity
    }
    
    # df$L5.hour[ i ]  <- this.L5$L5.hour
    # df$L5.value[ i ] <- this.L5$L5.mean.activity
  }
  df$processing.timestamp[ i ] <- Sys.time()
  if( this.error.flag == TRUE ){
    df$process.QC[i] <- "FAILED"
  } else {
    df$process.QC[i] <- "PASSED"
  }
  
}


# output the file of summary stats
write.csv( df, file = paste0( file.path( output.path, "non-parametrics-all.csv") ), row.names = FALSE )

