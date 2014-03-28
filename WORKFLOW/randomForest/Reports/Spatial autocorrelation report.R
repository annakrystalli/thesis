an.IDCV<-"model assess NC CV"
model<-"rf_i"

load(paste("~/Documents/TRAINING DATA/Models/randomForest/",an.IDCV,"1/forests/Spatial Autocorr ",
                                       model, ".Rdata", sep=""))

print("OC models")
print(moransOC.spp)
print("AC models")
print(moransAC.spp)
