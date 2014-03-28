rm(list=ls())
an.IDCV<-"model assess NC CV"

source('~/Documents/WORKFLOW/randomForest/Autocorrelation diagnostics functions.R', chdir = TRUE)

printXcorrReport(method="pearson", model="rf_i", an.IDCV)
printXcorrReport(method="spearman", model="rf_i", an.IDCV)


