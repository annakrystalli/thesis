# --- Demonstration of the method ----
require(car)
# -- Defines the categories -- 
cats <- c(1:5)
acc <- c( 1, 2, 3, 6, 17, 35, 75, 160, 310, 640, 1300, 2690 )
ab <- c( 50, 100, 150, 300, 850, 1750, 3750, 8000, 15500, 32000, 65000, 134500)

# Raw categories, as supplied, min and max of each state
mn <- c( 1, 4, 12, 51,  501)
mx <- c( 3, 11, 50, 500, 4000)

# Load up the data
dat1 <- states
dat2 <- raw

# A histogram of the data and models 
layout( matrix( c(1,2), ncol  =1) )
hist( dat1$state, freq = FALSE)  	# Actual data
hist( dat2$y, freq = FALSE)			# Model fits

# Recode the data - collapse first 4 states
rdat1 <- recodeDat1( dat1$state )   
rdat2 <- recodeDat1( dat2$y )

# Recoded categories, min, max and breaks
rmn1 <- c( 1, 4, 12, 26, 51, 126, 251, 501, 1001, 2001 )
rmx1 <- c(  3, 11, 25, 50, 125, 250, 500, 1000, 2000, 4000 )
bks1 <- c( 1, 4, 12,  51,  501, 4000 )

# Summarise the data
hdat1 <- table( factor( rdat1, levels = c(1:5) ))
hdat2 <- table( factor( rdat2, levels = c(1:5) ))

# fit a log-gamma distribution to the raw observations
op1 <- optGammaFun( hdat1, log(bks1) )
text( 2, 1, "Observations :- red = data; line = fit", cex = 0.75)
# fit a log-gamma distribiution to the model fits
op2 <- optGammaFun( hdat2, log(bks1) )
text( 3, 1, "Model fits :- red = data; line = fit", cex = 0.75)

# This is the mean based on the raw data, using 'accepted' values
sumDat <- table( factor( dat1$state, levels = c(1:12) ) )
sumDat <- sumDat / sum( sumDat)
rawMean <- sum(sumDat * acc )


# This is the mean based on the model data, using 'accepted' values
sumDat <- table( factor( dat2$y, levels = c(1:12) ) )
sumDat <- sumDat / sum( sumDat)
modelMean <- sum(sumDat * acc )


