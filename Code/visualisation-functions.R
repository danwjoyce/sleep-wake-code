# visualisation
require(ggplot2)
require(reshape2)

activityHeatmap <- function( mat.df, title.str = "Activity Heatmap" ) {
  
  # Produce a ggplot heatmap of 24 hour activity with midnight centered on the x-axis
  # This is a bit of a fudge, but centering on midnight seems the norm for
  # displaying these kinds of plots in the literature. 
  
  # Arguments : mat.df = the output of aggregating-functions.R : dayByHourMatrix()
  
  # append columnwise the activity matrix in [0 ... 23] hour format so we have [0 ... 23, 0 ... 23]
  # then take "middle" columns as a 24-hour block such that the 
  # new leftmost column is midday, center column is midnight and rightmost is 11am
  mat.temp <- cbind(mat.df, mat.df)
  mat.temp <- mat.temp[ , seq( 13, (13+23), by = 1 )  ]
  colnames(mat.temp) <- seq(1,24,by = 1)
  
  # melt into long format for ggplot's benefit
  m.df <- melt( mat.temp )
  colnames(m.df) <- c("Day","Hour","Activity")
  
  basictheme <- theme_minimal() + 
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 10) )
  
  ggplot( m.df, aes( x = Hour, y = Day, fill = Activity ) ) + 
    geom_tile() +
    scale_fill_viridis_c() +
    scale_x_continuous( breaks = seq(1,24,by = 1), labels = c( seq( 12, 23, by = 1 ), seq(0,11,by = 1) ) ) +
    ggtitle( title.str ) +
    basictheme
}
