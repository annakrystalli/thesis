rm(list=ls())
require(randomForest)

#DATA prep_________________________________________________________________________________
input.folder<-"/data/bop10ak/SATELLITE/PROCESSED/"

filenames<-list.files(path=paste(input.folder,
                                 "normalised.na", sep=""),all.files = FALSE)


x<-matrix(0,149919020 , 21)
x<-as.data.frame(x)
r.ind<-0

for (i in 1:length(filenames)){
  load(paste(input.folder,
            "normalised.na/", filenames[i],sep=""))
  
  
  
  r.end<-r.ind+dim(X.pred)[1]
  x[r.ind+1:r.end,]<-X.pred

  r.ind<-r.end
  
  if(i==length(filenames)){colnames(x)<-names(X.pred)
  save(x, file=paste(input.folder, "x.na_full_data.Rdata"))}
}

#_________________________________________________________________________________________
# $Id: rfImputeUnsupvsd.R,v 1.5 2013/01/16 07:39:21 tunenori Exp $
# Description:
#   Unsupervised Data Imputation using the proximity from Random Forests.
#
# Usage:
#	rfImpute.unsupvsd(x, iter=10)
#
# Arguments:
#	x: An unsupervised data frame or matrix, some containing 'NA's.
#	   Response vector is not needed.
#	iter: Number of iterations needed to run the imputation.
#
# Details:
#     	The algorithm starts by imputing 'NA's by using 'na.roughfix'. Then
#     	'randomForest' is called with the completed data. The proximity
#     	matrix from the randomForest is used to update the imputation of
#     	the 'NA's. 
#	Note that rfImpute(), developed by Andy Liaw, has not (yet) been
#	implemented for the unsupervised case.
#
# Value:
#     A data frame or matrix containing the completed data matrix, where
#     'NA's are imputed by using the proximity from randomForest. 
#
# See Also:
#	'rfImpute', 'na.roughfix'
#
# Example:
#
# library(randomForest)
# 
# data(iris)
# iris.na <- iris
# set.seed(111)
# ## artificially drop some data values.
# for (i in 1:4) iris.na[sample(150, sample(20)), i] <- NA
# x <- iris.na[,-5] # Remove the `Species'
# set.seed(222)
# irisImpute.unsupvsd <- rfImpute.unsupvsd(x)
# 
# $Id: rfImputeUnsupvsd.R,v 1.5 2013/01/16 07:39:21 tunenori Exp $

rfImput.unsupvsd <- function (x, iter=5){
  x.roughfix <- na.roughfix(x)
  rf.impute <- x
  
  while (iter){
    x.rf <- randomForest(x.roughfix, ntree=100)
    x.prox <- x.rf$proximity
    
    for (i in 1:ncol(x)){
      rf.impute[,i] <- nafix.prox(x[,i], x.roughfix[,i], x.prox)
    }
    diff.rel <- dist.rel(rf.impute, x.roughfix)
    if (diff.rel < 1e-5){
      break
    }else{
      x.roughfix <- rf.impute
      iter <- iter -1
    }
  }
  print(x.rf)
  return(rf.impute)
}

# Description:
# 	Return relative distance between `x.impute' to `x.org' 
# Arguments:
# 	x.impute: imputed data
#	x.org: original data
dist.rel <- function (x.impute, x.org){	  
  max.x <- lapply(abs(x.org), max) # for normalization of features size
  # `x.impute' and `x.org' may include factor elements
  if (FALSE){ # available for only numeric
    diff.x <- (x.impute - x.org) / max.x # normalize
    diff.rel <- sum(diff.x^2) / sum((x.org / max.x)^2) 
  }else{
    ncol.x <- length(max.x)
    mat.x.impute <- matrix(as.numeric(unlist(x.impute)), ncol=ncol.x)
    mat.x.org <- matrix(as.numeric(unlist(x.org)), ncol=ncol.x)
    max.numx <- as.numeric(unlist(max.x))
    diff.x <- sweep((mat.x.impute - mat.x.org), 2, max.numx, FUN="/") 
    size.org <- sweep(mat.x.org, 2, max.numx, FUN="/")
    diff.rel <- sum(diff.x^2) / sum(size.org^2)
  }
  cat ("diff.rel =", sum(diff.x^2), "/", sum(size.org^2), "=", diff.rel, "\n")
  return(diff.rel)
}


# Description:
#	Impute or revise NA elements using the data proximity.
# Arguments:
#	na.vales: data vector that includes NA; unchanged.
#	rough.vales: rough data vector to be replaced; NAs cannot include.
#	x.prox: data proximity matrix; each element is positive and <= 1.
nafix.prox <- function (na.vales, rough.vales, x.prox){
  if (length(na.vales) != length(rough.vales))
    stop("'na.vales' and 'rough.vales' must have the same length")
  else if (length(rough.vales) != ncol(x.prox))
    stop("'rough.vales' and 'x.prox' size incorrect")
  # NA imputation ONLY for NA data
  na.list <- which(is.na(na.vales))
  replaced.vales <- rough.vales
  for (i in 1:length(na.list)){
    j <- na.list[i]
    x.prox[j,j] <- 0 # ignore the weight of the data to be imputed.
    replaced.vales[j] <- kWeighted.mean (rough.vales, x.prox[,j])
  }
  return(replaced.vales)
}



# Description:
# 	Return k-neighbor weighted mean for numeric variables or
# 	most weighted frequent factor element for factor variables.
# Arguments:
#	value: vector; numeric or factor variables.
#	weight: vector; numeric.
#	k: the number of neighbors.
kWeighted.mean <- function (value, weight, k=10){
  if (missing(weight))
    w <- rep.int(1, length(value))
  else if (length(weight) != length(value))
    stop("'value' and 'weight' must have the same length")
  k <- min(k, length(value))
  if (is.numeric(value)){
    # weighted mean
    # ret <- sum(value * weight) / sum(weight)
    
    order.weight <- order(weight, decreasing = T)
    ww <- weight[order.weight]
    vv <- value[order.weight]
    ret <- sum(ww[1:k] * vv[1:k]) / sum(ww[1:k])
    
  }else if(is.factor(value)){ 
    wgt.sum <- tapply(weight, value, sum)
    # most weighted frequent factor element
    ret <- names(subset (wgt.sum, wgt.sum == max(wgt.sum)))
  }else{
    stop("'value' is neither numeric nor factor")
  }
  return(ret)
}




x.imp<-rfImput.unsupvsd(x=x, iter=10)

save(x.imp, file=paste(input.folder, "x.imp_full_data.Rdata"))