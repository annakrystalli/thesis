#Set up


rm(list = ls())
an.ID="No Geo Strata/"

source('~/Documents/WORKFLOW/AORS/AOR extract functions.R', chdir = TRUE)




for (spp in c("chel","cfin", "metrilu", "tem", "centrot")){
  AORextract(spp=spp,  b=NULL, a=0.95, y.plots=T, 
             an.ID=an.ID, years=1998:2010, no.plot=T, hists=F, season=F)}


for (spp in c("chel", "cfin","metrilu", "tem", "centrot")){
plotYearly(pt=14, y.plots=T, spp=spp, an.ID=an.ID,
           PR=T, no.plot=F, log="log")}

plotYearlyAll(pt=28, y.plots=F, input.folder=input.folder)


processAORdat(an.ID)

for (spp in c("chel", "cfin","metrilu", "tem", "centrot")){
interannualAOR(plot=F, spp=spp, xlim=c(0,7.5), pt=20,
               an.ID=an.ID, years=NULL, no.plot=F, PR=T, log="log")}