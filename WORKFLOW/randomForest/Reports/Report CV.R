

an.ID<-paste("model assess NC CV",1, "/", sep="")

load(file=paste("~/Documents/TRAINING DATA/Models/randomForest/",
                     an.ID,"forests/result overall summary.Rdata", sep=""))

print(results.mean)


