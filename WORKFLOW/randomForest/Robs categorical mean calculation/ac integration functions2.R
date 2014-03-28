

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
optGammaFun <- function( ds, breaks, initg=c(1,1), method="Nelder-Mead" ) {
  
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
  op <- optim(initg, optFun, method=method)
  J <- op$value
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
  
  return( list(J=J, shape = shape, rate = rate, mean = mean, var = var, probs = probs, ds = ds) )
  
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
optlnormFun <- function( ds, breaks , initl=c(1,1), method="Nelder-Mead") {
  
  # --- Function that is optimised, crudely based on least squares --- 
  optFun <- function( pars) {
    lmean <- pars[1]
    lvar <-pars[2]
    probs <- lnormFun( breaks, lmean, lvar )
    ssq <-  ( ds - probs )^2
    return( sum( ssq) )
  }
  
  ds <- ds / sum(ds)
  op <- optim( initl, optFun, method=method)
  J <- op$value
  n <- length(breaks)
  lmean <- op$par[1]
  lvar <- op$par[2]
  starts <-  breaks[1:(n-1)] 
  ends <-  breaks[ 2:n] 
  
  probs <- plnorm(ends, lmean, lvar) - plnorm(starts, lmean, lvar)
  
  x <- seq(min(breaks), max(breaks), by = (max(breaks) - min(breaks)) / 100)
  fx <-  plnorm(x[2:length(x) ], lmean, lvar) - plnorm(x[1:(length(x)-1)], lmean, lvar)
  
  mean <- exp( lmean + 0.5 * lvar )
  var <- ( exp( lvar ) - 1 ) * exp( 2 * lmean + lvar )
  
  plot( ( breaks[1:(length(breaks)-1) ]  + breaks[ 2:length(breaks)  ] ) / 2, probs, type = "l" , log = "x",
        xlab = "State", ylim = c(0,1), main=paste(initl$a, initl$b, sep="-"))
  points( ( breaks[1:(length(breaks)-1) ]  + breaks[ 2:length(breaks)  ] ) / 2, ds, col= "red" )
  text( 40*f, 1, paste("Model fits :- red = data; line = fit; Mean =", signif(mean,3)), cex = 0.75)
  
  
  return( list(J=J, lmean = lmean, lvar = lvar, mean = mean, var = var, probs = probs, ds = ds) )	
}


