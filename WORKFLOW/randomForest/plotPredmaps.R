#Set up
rm(list = ls())
an.ID="No Geo Strata/"

require(png)

source('~/Documents/WORKFLOW/randomForest/plotPredmaps functions intecol.R', chdir = TRUE)

#_______________________________________________________________________________________________________

#Pred mMAP........................................................................

year.s<-2004
spp.v<-c("chel","cfin", "metrilu", "centrot", "tem")
cols.matrix<-matrix(c( "antiquewhite2",  "darkgoldenrod4",
                       "honeydew2", "aquamarine4", 
                       "lavenderblush2","palevioletred4",
                       "thistle1", "purple3",
                       "mistyrose3","indianred4"),
                    nrow=5, ncol=2, byrow=TRUE)


for(j in 4){
for(i in 1:length(year.s)){
  
  predMultiMapIntecol(year=year.s[i], spp=spp.v[j], spp.col=cols.matrix[j,], pt=20, an.ID=an.ID)
}}
