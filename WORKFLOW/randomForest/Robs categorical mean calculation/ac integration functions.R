

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
  
  return( list(J=J, lmean = lmean, lvar = lvar, mean = mean, var = var, probs = probs, ds = ds) )	
}



initSearchOpt <- function(x, inits, breaks, spp, method="Nelder-Mead"){

    if(is.null(dim(x))){}else{x<-x$pred.ac[x$pred.oc==1]}  
  
    BKS<-list(AC5=breaks[-1,1], AC4=breaks[c(-1,-3),1],AC3=breaks[c(-1,-3,-4),1], 
              AC2=breaks[c(-1,-3,-4, -5),1], AC1=breaks[c(-1,-3,-4, -5, -6),1])
    AC.IDS<-list(AC5=1:5, AC4=c(1,1,2:4),AC3=c(1,1,2,3), AC2=c(1,1,2), AC1=c(1,1))
    
    nAC<-5
    
    bks<-BKS[[paste("AC",nAC, sep="")]]
    ACids<-AC.IDS[[paste("AC",nAC, sep="")]]

    while(is.finite(skewness(x)[1]) && skewness(x)[1]<0){
      nAC<-nAC-1
      ACids<-AC.IDS[[paste("AC",nAC, sep="")]]
      x<-as.numeric(factor(ACids[factor(x,levels = c(1:(nAC+1)))], levels = c(1:nAC)))
      }

      bks<-BKS[[paste("AC",nAC, sep="")]]
      ACids<-AC.IDS[[paste("AC",nAC, sep="")]]

      if(spp %in% c("tem", "centrot")){f=50}else{f=1}

          x<-factor(x, levels = c(1:nAC))
    
          Means<-NULL
            
          for (i in 1:dim(inits)[1]){
            
            opt<-try(optlnormFun(table(x),
                                 bks*f, inits[i,], method), silent=T)
            
            if(class(opt)=="try-error"){ 
              Means<-c(Means,NA)}else{Means<-c(Means, opt$mean)}}
  
      mean<-median(Means[is.finite(Means)])
 
  
  return(list(mean=mean, nAC=nAC))
  
}
      