# non-parapmetric summary stats for actigraphy data
# Designed to be implemented without the need for periodogram-functions.R


interdailyStability <- function( mat.df, P = 24 ) {
  # computes inter-daily stability -- default to period of 24 hours
  # See W. Witting, I.H. Kwa, P. Eikelenboom, M. Mirmiran, D.F. Swaab,
  #   Alterations in the circadian rest-activity rhythm in aging and 
  #   Alzheimer's disease, Biological Psychiatry, Volume 27, Issue 6, 1990,
  #   pp. 563-572
  
  # Arguments:
  #   mat.df = activity matrix in [days,hours] form e.g. from 
  #             dayByHourMatrix() in aggregating-functions.R
  
  # Returns:
  #   interdaily stability (IS) as a real number in [0,1]
  #   where 0 indicates no stability at all (e.g. on pure noise)
  #   and 1 is perfect stability of the P period rhythm/cycle
  
  X_v <- c(t(mat.df))
  
  N <- length( X_v )
  K <- N/P
  
  # Pad out X_v with NA for to ensure rectangular matrix
  X_v <- c(X_v, rep.int(NA, ceiling(K) * P - N ))
  # arrange in row-major order a matrix of P columns and K rows
  X_m <- matrix( X_v, byrow = TRUE, ncol = P )
  
  # mean of all data, excluding NAs
  X_bar <- mean( X_m, na.rm = TRUE )
  # column means for activity matrix
  X_h <- apply( X_m, 2, mean, na.rm = TRUE )
  
  # adjust for actual data, i.e. excluding NAs
  N.included <- N - length( which( is.na( X_v ) ) )
  
  numer <- N.included * sum( (X_h - X_bar)^2, na.rm = TRUE )
  denom <- P * sum( (X_v - X_bar)^2, na.rm = TRUE )
  
  return( numer/denom )
}

intradailyVariability <- function( mat.df, LOCF = TRUE ) {
  # computes intra-daily variability
  # See W. Witting, I.H. Kwa, P. Eikelenboom, M. Mirmiran, D.F. Swaab,
  #   Alterations in the circadian rest-activity rhythm in aging and 
  #   Alzheimer's disease, Biological Psychiatry, Volume 27, Issue 6, 1990,
  #   pp. 563-572
  
  # Arguments:
  #   mat.df = activity matrix in [days,hours] form e.g. from 
  #             dayByHourMatrix() in aggregating-functions.R
  
  # Returns:
  #   intra-daily variability (IV) as a real number in [0,2]
  #   where 0 indicates no within day variability 
  #   at all (e.g. a perfectly sinusoidal activity profile)
  #   and ~2 is the value obtained from an activity matrix that is
  #   complete noise
  
  # NOTE: because data may have dropout (e.g. missing hours of activity)
  #       as NAs, the default is to use "last observation carried forward"
  #       to make the first derivative (numerator - see below) smoother
  #       Beware this may be an assumption you may not want to make
  #       -- that is, if no data for specific hour(s), assume NO difference 
  #          from the last recorded hour's activity
  
  X_v <- c(t(mat.df))
  
  if( LOCF == TRUE ) {
    # replace missing data NAs with last observation
    X_v <- LOCF_NA( X_v )
  }
  
  # X_v no longer has any "holes" with NA / missing values, but
  # it may have leading/trailing NAs because recording started/stopped
  # at times different to midday or midnight
  # We now remove any remaining NAs (which can only
  # be leading/trailing by virtue of the LOCF procedure above)
  idx.na <- which(is.na(X_v))
  if( length(idx.na > 0)  ) {
    X_v <- X_v[ -idx.na ]
  }
  
  X_bar <- mean( X_v, na.rm = TRUE )
  N <- length( X_v ) - length( which( is.na( X_v ) ) )
  
  # numerical approx to first derivative of time-series X_v
  dX_v <- diff( X_v )
  
  # numerator / denominator of Eq (2) in Witting et al, 1990
  numer <- N * sum( dX_v^2 )
  denom <- (N-1) * sum( (X_bar - X_v)^2 )
  
  return( numer/denom )
}

compute_M10 <- function( X_m ) {
  # M10 -- total activity of most active hours : use the algorithm in 
  #   Van Someren, E. J., Swaab, D. F., Colenda, C. C., Cohen, W., 
  #   McCall, W. V., & Rosenquist, P. B. (1999). Bright light therapy: improved 
  #   sensitivity to its effects on rest-activity rhythms in Alzheimer patients 
  #   by application of nonparametric methods. 
  #   Chronobiology international, 16(4), 505-518.

  # Arguments:
  #   X_m : activity matrix organised in [day, hour] format e.g. 
  #         from the function dayByHourMatrix() in aggregating-functions.R
  
  X_h = apply( X_m, 2, mean, na.rm = TRUE )  
  df <- filterHour_ForwardBack(X_h, 10)
  
  # now, df$filterHourMean is the rolling 10 hour window
  # average, and the corresponding df$Hour is the midpoint
  # of that 10 hour window
  df <- df[ order( df$filterHourMean, decreasing = TRUE ), ]
  
  return( list( 
            M10.hour = df$Hour[1], 
            M10.mean.activity = df$filterHourMean[1] 
            )
          )
}

compute_L5 <- function( X_m ) {
  # L5 -- total activity of most active hours : use the algorithm in 
  #   Van Someren, E. J., Swaab, D. F., Colenda, C. C., Cohen, W., 
  #   McCall, W. V., & Rosenquist, P. B. (1999). Bright light therapy: improved 
  #   sensitivity to its effects on rest-activity rhythms in Alzheimer patients 
  #   by application of nonparametric methods. 
  #   Chronobiology international, 16(4), 505-518.
  
  # Arguments:
  #   X_m : activity matrix organised in [day, hour] format e.g. 
  #         from the function dayByHourMatrix() in aggregating-functions.R
  
  X_h = apply( X_m, 2, mean, na.rm = TRUE )  
  df <- filterHour_ForwardBack(X_h, 5)
  
  # now, df$filterHourMean is the rolling 5 hour window
  # average, and the corresponding df$Hour is the midpoint
  # of that 5 hour window
  # Order in ascending order and take the smallest value
  df <- df[ order( df$filterHourMean, decreasing = FALSE ), ]
  
  return( list( 
    L5.hour = df$Hour[1], 
    L5.mean.activity = df$filterHourMean[1] 
  )
  )
}


# ---------------- Helpers -----------------------------------------------------
LOCF_NA <- function(x) {
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v)+1]
}

filterHour <- function(x, n){
  return(
    stats::filter(x, rep(1 / n, n), method = "convolution", circular = TRUE, sides = 1)
  )
}

filterHour_ForwardBack <- function( x, n ) {
  
  # Arguments:
  #   x : vector of hourly means as a sequence hour = [0,1,2, .... , 23]
  #   m : number of hours to compute rolling average
  
  # filter backward, then forward
  # (this removes phase shift by averaging for/backward together)
  f.x.1 <- rev( filterHour( rev( x ), n) ) 
  f.x.2 <- filterHour( x, n)
  f.x   <- 0.5 * (f.x.1 + f.x.2)
  
  df <- data.frame(
    Hour = seq(0,23,by = 1),
    HourMean = x,
    filterHourMean = f.x
  )
  
  return( df )  
}

