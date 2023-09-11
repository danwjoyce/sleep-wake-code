# Implementations of different periodogram functions
# See Sokolove and Bushell (1978) for implementation of Enright (1965) 

# Note: these functions are coded for clarity / to transparently marry with 
# computations in the Sokolove and Bushell paper.  They likely can be implemented
# more efficiently using vectorisation and other idiomatic R code, but 
# for transparency/ease of verifying, I've chosen to implement them more explicitly.


# ---------- Enright Periodogram -----------------------------------------------
# Reference:
#   Sokolove, P. G., & Bushell, W. N. (1978). The chi square periodogram: its 
#     utility for analysis of circadian rhythms. 
#     Journal of theoretical biology, 72(1), 131-160.

computeMean_X_h_p <- function( X, h, p, K, na.rm = TRUE ) {
  # takes a vector X of activity values and computes Eq 1 of Sokolove and Bushell (1978)
  # Arguments:
  #   X = vectorised activity matrix
  #   h = which hour to compute X^{bar}_{h,p}
  #   p = period in integer hours (e.g. 24 hours, 12 hours)
  #   K = number of days
  
  # see Eq 1 of S&B (1978) : select values (in X) indexed by (h + jp)
  j <- seq(0,(K-1), by = 1)
  # fetch values from (h + jp)
  X_h_p <- X[ h + j*p ]
  # here, we adjust the constant 1/K (Eq 1) to account for missing data
  # which often occurs at the start and end of actigraphy (i.e. data collection
  # rarely starts and ends at midnight)
  if( na.rm == TRUE ) {
    # if na.rm = TRUE, remove any NA 
    X_h_p <- X_h_p[ !is.na( X_h_p ) ]
  } 
  this.K <- length( X_h_p )
  # In computing the mean, leave na.rm = TRUE in case the user genuinely wants 
  # to treat NAs as values (inflating this.K)
  return(
    (1/this.K) * sum( X_h_p, na.rm = TRUE )  
  )
}

computeEnright_Ap <- function( X, p, adjust.for.NA = TRUE ) {
  # Compute the Enright 1965 A_p (root mean square of deviations
  # of each hour's mean activity from global mean)
  # See equation 2 of Sokolove and Bushell (1978) pp. 137
  
  # Arguments:
  #   X = activity matrix, arranged [days, hours] -- e.g. output from
  #       dayByHourMatrix()
  #   p = integer period (e.g. integer between 1 ... 24)
  #   adjust.for.NA = TRUE ensures we adjust hour means of X to account for 
  #                   missing data e.g. first / last day having missing hours
  
  P <- ncol( X ) # number of hours in each day
  K <- nrow( X )
  # convert matrix [day, hour] into a vector *by row* e.g.
  # (day1:hour0, day1:hour1, ... day1:hour23, day2:hour0, day2:hour1, ... day2:hour23, ....)
  # See Sokolove and Bushell for why this helps
  X <- c(t(X)) 
  N <- length( X )
  
  # means of activity by HOUR over ALL DAYS
  X_bar_h_p <- rep(NA,P)
  # for every hour (h), using period (p) compute mean X_h_p
  for( h in 1:P ) {
    X_bar_h_p[ h ] <- computeMean_X_h_p( X, h, p, K, na.rm = adjust.for.NA )  
  }
  
  # mean X_bar_p (see Eq 2 of S&B, 1978)
  X_bar_p = (1/p) * sum( X_bar_h_p )
  
  # compute Equation 2, root mean square amplitude for each hour
  A_p <- sqrt( (1/P) * sum( (X_bar_h_p - X_bar_p)^2 ) )
  
  return( A_p )
}

periodogram_Enright <- function( X, periods = seq(14,34,by = 1), adjust.for.NA = TRUE ) {
  # Compute a complete Enright periodogram for periods (default : 14 through 34) 
  # removing NAs from the estimation of hourly means by default
  # NOTE : will return NaNs for periods which span NAs in the data
  pgram <- data.frame(
    Period = periods,
    Value = rep( NA, length(periods) )
  )
  
  for( i in 1:length( periods ) ) {
    pgram$Value[ i ] <- computeEnright_Ap( X, p = periods[ i ], adjust.for.NA )
  }
  return( pgram )
}

# ---------- Chi Square Periodogram --------------------------------------------

# See : 
# Tackenberg, M. C., & Hughey, J. J. (2021). The risks of using the 
# chi-square periodogram to estimate the period of biological rhythms. 
# PLoS computational biology, 17(1), e1008567.

computeQ_P <- function( X_v, N, P, K ) {
  # This implementation follows conventions in 
  # Tackenberg, M. C., & Hughey, J. J. (2021). 
  
  # This is because I found it difficult to use the 
  # Sokolove & Bushell equations for the chi-square periodogram.
  # So, in this function, the variables N, K, P etc are 
  # not the same as S&B (1978) - see Tackenberg & Hughey (2021) instead
  
  # Arguments : 
  #     X_v = vectorised [day,hour] activity matrix
  #     N = length of X_v
  #     P = period to compute (e.g. 17 hours)
  #     K = number of rows to construc from X_v -- see below
  
  
  # arrange in row-major order a matrix of P columns and K rows
  # such that P * K + D = N
  # where N is total number of time points, P is period, N is the total
  # number of samples (i.e. total elements in X) and
  # D is the number of data points "dropped"
  # i.e. D = N - (P * K)
  
  X_included <- X_v[ 1:(K*P) ]
  X_m <- matrix( X_included, byrow = TRUE, ncol = P, nrow = K)
  X_bar <- mean( X_included, na.rm = TRUE )
  N_included <- length(X_included) - length( which( is.na( X_included ) ) )
  
  # column means of X_m
  X_h <- apply( X_m, 2, mean, na.rm = TRUE )
  
  numer <- K * N_included * sum( (X_h - X_bar)^2 )
  denom <- sum( (X_included - X_bar)^2, na.rm = TRUE )
  
  return( numer / denom )
}

computeQ_P_Greedy <- function( X_v, N, P, K ) {
  
  # This implementation follows GREEDY algorithm described in 
  # Tackenberg, M. C., & Hughey, J. J. (2021). 
  
  # Arguments : 
  #     X_v = vectorised [day,hour] activity matrix
  #     N = length of X_v
  #     P = period to compute (e.g. 17 hours)
  #     K = number of rows to construc from X_v -- see below
  
  # Pad out X_v with NA for to ensure rectangular matrix
  X_v <- c(X_v, rep.int(NA, ceiling(K) * P - N ))
  
  # arrange in row-major order a matrix of P columns and K rows
  X_m <- matrix( X_v, byrow = TRUE, ncol = P )
  
  X_bar <- mean( X_m, na.rm = TRUE )
  
  # column means of X_m
  X_h <- apply( X_m, 2, mean, na.rm = TRUE )
  
  N.included <- N - length( which(is.na(X_v)) )
  K.included <- N.included / P
  
  numer <- K.included * N.included * sum( (X_h - X_bar)^2, na.rm = TRUE )
  denom <- sum( (X_m - X_bar)^2, na.rm = TRUE )
  return(
    numer / denom
  )
}

periodogram_ChiSquare <- function( X, periods = seq(14,34,by = 1), greedy = FALSE ) {
  # compute Chi Square periodogram (see computeQ_P for details)
  # over sequence of periods.
  
  # Arguments:
  #   X = [day, hours] matrix of activity e.g. from dayByHourMatrix()
  #   periods = vector of periods to test
  #   greedy == TRUE - use 'greedy' algorithm to avoid discontinuity
  #             See computeQ_P_Greedy for details/reference
  
  pg <- data.frame( Period = periods, 
                    Value = rep(NA, length( periods ) )
  )
  
  # vectorise the [days, hours] matrix
  X_v = c(t( X ))
  N <- length(X_v)
  
  for( i in 1:length(periods) ) {
    this.P <- periods[ i ]  
    if( greedy == TRUE ) {
      # used in computation of Q_P statistic
      this.K <- N / this.P
      pg$Value[ i ] <- computeQ_P_Greedy( X_v, N, this.P, this.K )
    } else {
      # conservative rule for K 
      this.K <- floor( N/this.P )
      pg$Value[ i ] <- computeQ_P( X_v, N, this.P, this.K)  
    }
  }
  
  return( pg )
}

add_pValue_ChiSquare <- function( pg ) {
  # Takes the chi square periodogram and adds a column of P values for
  # the approximate chi-square statistic (Q_p, computed by periodogram_ChiSquare)
  
  # Arguments:
  #   pg = data.frame of Periods and Values (Q_p) - e.g. from periodogram_ChiSquare
  
  # WARNING : implemented for completeness but beware with few days ( < 10) the 
  #           robustness of the chi-square P value is dubious.
  
  pg$log.Pvalue <- rep( NA, nrow(pg) )
  for( i in 1:nrow( pg ) ) {
    pg$log.Pvalue[i] <- pchisq( pg$Value[i], df = pg$Period[i] - 1, log.p = TRUE, lower.tail = FALSE )  
  }
  return( pg )
  
}

# ---------- Other Helpers -----------------------------------------------------
scale01 <- function(x){
  # scale the matrix / vector x so that all values in range [0,1]
  # where 0 == min(x) and 1 == max(x)
  max.a <- max(x, na.rm = TRUE)
  min.a <- min(x, na.rm = TRUE)
  return(
    (x-min.a)/(max.a - min.a)
  )
}

invertActivity <- function( X ) {
  # Takes a matrix X in [day,hour] format and inverts values so that
  # highest value of an element becomes the lowest
  # and vice versa. 
  # This has the effect of turning an activity matrix into an 
  # "inactivity" matrix -- so the highest values are then
  # periods of rest, rather than activity
  # Note this requires us to scale the matrix X
  
  return( 1 - scale01(X) )
}

