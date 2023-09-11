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
  IS = rep(NA, length( file.list ) ),
  IV = rep(NA, length( file.list ) ),
  M10.hour = rep(NA, length( file.list ) ),
  M10.value = rep(NA, length( file.list ) ),
  L5.hour = rep(NA, length( file.list ) ),
  L5.value = rep(NA, length( file.list ) )
)


for( i in 1:length( file.list) ) {

  # 1. load data from CSV file, just as for periodogram
  this.sub <- file.list[ i ]
  d <- readParticipantCSV( data.path, this.sub )

  # 2. Aggreagte and convert to [day,hour] matrix format
  mat.df <- dayByHourMatrix( aggregateHours( d ) )
  act.plot <- activityHeatmap( mat.df, file.list[i] )
  ggsave( filename = paste0( file.path( output.path, file.list[i] ), ".png" )
          , bg = "white" )
  
  # 3. Compute summary statistics
  df$IS[ i ]  <- interdailyStability( mat.df )
  df$IV[ i ]  <- intradailyVariability( mat.df )
  
  this.M10 <- compute_M10( mat.df )
  this.L5  <- compute_L5( mat.df )
  
  df$M10.hour[ i ]  <- this.M10$M10.hour
  df$M10.value[ i ] <- this.M10$M10.mean.activity
  
  df$L5.hour[ i ]  <- this.L5$L5.hour
  df$L5.value[ i ] <- this.L5$L5.mean.activity

  df$processing.timestamp[ i ] <- Sys.time()
}

# output the file of summary stats
write.csv( df, file = paste0( file.path( output.path, "non-parametrics-all.csv") ), row.names = FALSE )

