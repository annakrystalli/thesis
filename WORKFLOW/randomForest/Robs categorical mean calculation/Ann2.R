# ------- Demonstrates numerical integration of DS data ---- 


rm( list = ls() )


load("~/Dropbox/Anna PhD/log-gamma approximation/gamma data.RData")


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
	var <- ( exp( lvar ) - 1 ) * exp( 2 * lmean + lvar )
	
	return( list( lmean = lmean, lvar = lvar, mean = mean, var = var, probs = probs, ds = ds) )	
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
optGammaFun <- function( ds, breaks, init = c(1, 1) ) {
	
	# --- Function that is optimised, crudely based on least squares --- 
	optFun <- function( pars) {
		 shape <- pars[1]
		 rate <-pars[2]
		 probs <- GammaFun( breaks, shape, rate)
		 ssq <-  ( ds - probs )^2
		 
		 return( sum( ssq) )
	}
	
	ds <- ds / sum(ds)
	op <- optim( init, optFun)
	n <- length(breaks)
	shape <- op$par[1]
	rate <- op$par[2]
	starts <-  breaks[1:(n-1)] 
	ends <-  breaks[ 2:n] 
	
	probs <- pgamma(ends, shape, rate) - pgamma(starts, shape, rate)
	
	x <- seq(min(breaks), max(breaks), by = (max(breaks) - min(breaks)) / 100)
	fx <-  pgamma(x[2:length(x) ], shape, rate) - pgamma(x[1:(length(x)-1)], shape, rate)
	plot( ( breaks[1:(length(breaks)-1) ]  + breaks[ 2:length(breaks)  ] ) / 2, probs, type = "l" , log = "x",
		xlab = "State", ylab = "Proportion", ylim = c(0,1))
	points( ( breaks[1:(length(breaks)-1) ]  + breaks[ 2:length(breaks)  ] ) / 2, ds, col= "red" )
	mean <- shape / rate
	var <- shape / rate^2
	
	return( list(shape = shape, rate = rate, mean = mean, var = var, probs = probs, ds = ds) )
	
}




#Fit log-gamma distribution
op.rf<-optGammaFun(table(factor(ac.pred, levels = c(1:5))),
                   log(bks), init = c(2,2) )



h <- table(ac.pred)
h <- h / sum(h)
plot( seq(1:5), as.numeric( h) , xlab = "State", ylab = "Frequency", ylim = c(0,1) )
# Crappy fit
lines( seq(1:5), op.rf$probs , lty = "22", col = "red")


# Try 3 states
bks2 <- c(1, 51, 501, 4000)
ac.pred2 <- ac.pred
idx <- which( ac.pred2 == 3 )
ac.pred2 [ idx ] <- 1
idx <- which( ac.pred2 == 2 )
ac.pred2 [ idx ] <- 1
idx <- which( ac.pred2 == 4 )
ac.pred2 [ idx ] <- 2
idx <- which( ac.pred2 == 5 )
ac.pred2 [ idx ] <- 3

op.rf1<-optGammaFun(table(factor(ac.pred2, levels = c(1:3))),
                   log(bks2), init = c( 1, 1) )
h <- table(ac.pred2)
h <- h / sum(h)
plot( seq(1:3), as.numeric( h ), xlab = "State", ylab = "Frequency", ylim = c(0,1) )
# Not bad
lines( seq(1:3), op.rf1$probs , lty = "22", col = "red")



#Crappy in the tails
op.ln <- optlnormFun( table(factor(ac.pred, levels = c(1:5))),
                   bks )
                
                
# Fits nicely   
op.ln1 <- optlnormFun( table(factor(ac.pred2, levels = c(1:3))),
                   bks2 )



# compare parameters:

#Log-gamma:
op.rf1
exp(op.rf1$mean)


#log-norm
op.ln1

# Note that geometric means are very similar, which is good. 








bks0<-c(0, bks0)

all.pred2<-c(0,1,1,1,2,3)[factor(all.pred, levels=0:5)]
oc<-sum(all.pred2!=0)/length(all.pred2)


op.rf0<-optGammaFun(table(factor(all.pred2, levels = c(0:3))),
                    log(bks0), init = c( 6, 6) )
h <- table(all.pred2)
h <- h / sum(h)
plot( seq(1:3), as.numeric( h ), xlab = "State", ylab = "Frequency", ylim = c(0,1) )
# Not bad
lines( seq(1:3), op.rf0$probs , lty = "22", col = "red")



#Crappy in the tails
op.ln <- optlnormFun( table(factor(all.pred, levels = c(0:5))),
                      bks )


# Fits nicely   
op.ln0 <- optlnormFun( table(factor(all.pred2, levels = c(0:3))),
                       bks0 )



# compare parameters:

#Log-gamma:
op.rf0
exp(op.rf0$mean)


#log-norm
op.ln0

# Note that geometric means are very similar, which is good. 

