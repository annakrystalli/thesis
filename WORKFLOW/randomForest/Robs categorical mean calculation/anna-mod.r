# ------- Demonstrates numerical integration of DS data ---- 


rm( list = ls() )


# --- Custom function to create binned data ---
robhist <- function( x, breaks) {		
		xmin <- min(x)
		xmax <- max(x)
		step <- (xmax - xmin) / breaks 
		intervals <- seq(xmin, xmax, by = step)
		countfun <- function( i ) length( intersect( which( x > intervals[i]), which( x < intervals[i+1] ) ) )
		dx <- sapply( c(1: (length( intervals) - 1) ), countfun)
		return( list( counts = dx, breaks = intervals) )	
	}


# --- Integrates log-normal ----
lnormFun <- function( breaks, lmean, lvar ) {
		
		n <- length(breaks)	
		starts <- breaks[1:(n-1)] 
		ends <-  breaks[ 2:n]  
		probs <- plnorm( ends, lmean, lvar) - plnorm( starts, lmean, lvar )
		return( probs)
	
	}
	
	
# --- Optimizes the lognormal function for data and given breaks --- 	
optlnormFun <- function( ds, breaks ) {
	
	# --- Function that is optimised, crudely based on least squares --- 
	optFun <- function( pars) {
		 lmean <- pars[1]
		 lvar <-pars[2]
		 probs <- lnormFun( breaks, lmean, lvar )
		 ssq <-  ( ds - probs )^2
		 return( sum( ssq) )
	}
	
	ds <- ds / sum(ds)
	op <- optim( c(1,1), optFun)
	n <- length(breaks)
	lmean <- op$par[1]
	lvar <- op$par[2]
	starts <-  breaks[1:(n-1)] 
	ends <-  breaks[ 2:n] 
	
	probs <- plnorm(ends, lmean, lvar) - plnorm(starts, lmean, lvar)
	
	x <- seq(min(breaks), max(breaks), by = (max(breaks) - min(breaks)) / 100)
	fx <-  plnorm(x[2:length(x) ], lmean, lvar) - plnorm(x[1:(length(x)-1)], lmean, lvar)
	plot( ( breaks[1:(length(breaks)-1) ]  + breaks[ 2:length(breaks)  ] ) / 2, probs, type = "l" , log = "x",
		xlab = "State", ylim = c(0,1))
	points( ( breaks[1:(length(breaks)-1) ]  + breaks[ 2:length(breaks)  ] ) / 2, ds, col= "red" )
	mean <- exp( lmean + 0.5 * lvar )
	var <- ( exp( lvar ) - 1 ) * exp( 2 * shape + lvar )
	
	return( list(shape = shape, lvar = lvar, mean = mean, var = var, probs = probs, ds = ds) )	
}


# ---- Integrates Gamma Function --- 
GammaFun <- function( breaks, shape, rate ) {
		n <- length(breaks)
		starts <- breaks[1:(n-1)] 
		ends <-  breaks[ 2:n]  
		probs <- pgamma(ends, shape, rate) - pgamma(starts, shape, rate)
		#probs <- plnorm( ends, shape, rate) - plnorm( starts, shape, rate )
		return( probs )
	}
	

# --- Optimizes the gamma function for data and given breaks --- 	
optGammaFun <- function( ds, breaks ) {
	
	# --- Function that is optimised, crudely based on least squares --- 
	optFun <- function( pars) {
		 shape <- pars[1]
		 rate <-pars[2]
		 probs <- GammaFun(breaks, shape, rate)
		 ssq <-  ( ds - probs )^2 / probs
		 
		 return( sum( ssq, na.rm= T) )
	}
	
  ds <- na.omit(ds)
	ds <- ds / sum(ds)
	op <- optim(c(1,1), optFun)
	n <- length(breaks)
	shape <- op$par[1]
	rate <- op$par[2]
	starts <-  breaks[1:(n-1)] 
	ends <-  breaks[ 2:n]
	
	probs <- pgamma(ends, shape, rate) - pgamma(starts, shape, rate)
	
	x <- seq(min(breaks), max(breaks), by = (max(breaks) - min(breaks)) / 100)
	fx <-  pgamma(x[2:length(x) ], shape, rate) - pgamma(x[1:(length(x)-1)], shape, rate)
	mean <- shape / rate
	var <- shape / rate^2
	
	return( list(shape = shape, rate = rate, mean = mean, var = var, probs = probs, ds = ds) )
	
}


# --- Clunky function, explicitly recodes the data: first 3 classes are combined --- 
recodeDat1 <- function( x ) {
	recDat <- recode( x, "c(1, 2, 3 ) = 1" )
	recDat <- recode( recDat, " 4 = 2" )
	recDat <- recode( recDat, " c(5,6) = 3" )
	recDat <- recode( recDat, " c(7,8,9) = 4" )
	recDat <- recode( recDat, " c(10,11,12) = 5" )
	return( recDat )
	}

# --- Clunky function, explicitly recodes the data: first 4 classes are combined --- 
recodeDat2 <- function( x ) {
	recDat <- recode( x, "c(1, 2, 3, 4 ) = 1" )
	recDat <- recode( recDat, " 5 = 2" )
	recDat <- recode( recDat, " 6 = 3" )
	recDat <- recode( recDat, " 7 = 4" )
	recDat <- recode( recDat, " 8 = 5" )
	recDat <- recode( recDat, " 9 = 6" )
	recDat <- recode( recDat, " 10 = 7" )
	recDat <- recode( recDat, " 11 = 8" )
	recDat <- recode( recDat, " 12 = 9" ) 
	return( recDat )
	}





# --- Demonstration of the method ----

# -- Defines the categories -- 
cats <- c(1:12)
acc <- c( 1, 2, 3, 6, 17, 35, 75, 160, 310, 640, 1300, 2690 )
ab <- c( 50, 100, 150, 300, 850, 1750, 3750, 8000, 15500, 32000, 65000, 134500)

# Raw categories, as supplied, min and max of each state
mn <- c( 1, 2, 3, 4, 12, 26, 51, 126, 251, 501, 1001, 2001)
mx <- c( 1, 2, 3, 11, 25, 50, 125, 250, 500, 1000, 2000, 4000)


# Load up the data
dat1 <- read.csv( "states.csv")
dat2 <- read.csv( "raw.csv")

# A histogram of the data and models 
layout( matrix( c(1,2), ncol  =1) )
hist( dat1$state, freq = FALSE)		# Actual data
hist( dat2$y, freq = FALSE)			# Model fits

# Recode the data - collapse first 4 states
rdat1 <- recodeDat1( dat1$state ) 	
rdat2 <- recodeDat1( dat2$y )

# Histogram of the recoded data
layout( matrix( c(1,2), ncol  =1) )
hist( rdat1, freq = FALSE, breaks = seq(1:12))
hist( rdat2, freq = FALSE, breaks = seq(1:12))

# Recoded categories, min, max and breaks
rmn1 <- c( 1, 4, 12, 26, 51, 126, 251, 501, 1001, 2001 )
rmx1 <- c(  3, 11, 25, 50, 125, 250, 500, 1000, 2000, 4000 )
bks1 <- c( 1, 4, 12, 26, 51, 126, 251, 501, 1001, 2001 , 4000 )
bks2 <- c( 1, 12, 26, 51, 126, 251, 501, 1001, 2001 , 4000 )



# Summarise the data
hdat1 <- table( factor( rdat1, levels = c(1:10) ))
hdat2 <- table( factor( rdat2, levels = c(1:9) ))

# fit a log-gamma distribution to the raw observations
op1 <- optGammaFun( hdat1, log(bks1) )
text( 2, 1, "Observations :- red = data; line = fit", cex = 0.75)
# fit a log-gamma distribiution to the model fits
op2 <- optGammaFun( hdat2, log(bks2) )
text( 3, 1, "Model fits :- red = data; line = fit", cex = 0.75)

# This is the mean based on the raw data, using 'accepted' values
sumDat <- table( factor( dat1$state, levels = c(1:12) ) )
sumDat <- sumDat / sum( sumDat)
rawMean <- sum(sumDat * acc )


# This is the mean based on the model data, using 'accepted' values
sumDat <- table( factor( dat2$y, levels = c(1:12) ) )
sumDat <- sumDat / sum( sumDat)
modelMean <- sum(sumDat * acc )




cat( "\t Arithmetic Mean based on 'accepted' values and observed data:", rawMean, "\n", 
"\t Arithmetic Mean based on 'accepted' values and model data:", modelMean, "\n", 
"\t Arithmetic Mean based on numerical integration of observed distribution: ", exp( op1$mean + 0.5 * op1$var), "\n",
"\t Arithmetic Mean based on numerical integration of model data: ", exp( op2$mean + 0.5 * op2$var), "\n" ) 



