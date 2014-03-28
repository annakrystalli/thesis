require(geoR)

dat<-All.data.abundance

month<-10
year<-2005  
spp<-"chel"

krig.grid<-expand.grid(lat=seq(51,61, len=112), lon=seq(-4,11, len=92))
x$year==2005 & x$month==10

vario.dat<-as.geodata(dat[dat$month==month & dat$year==year,], 
                      coords.col=2:3, data.col=9)

plot(vario.dat)
vario<-variog(vario.dat, 
              estimator.type="modulus", 
              unit.angle="degrees")

ini.vals <- expand.grid(seq(51,61, len=10), seq(-4,11, len=10))
vfit <- variofit(vario, ini = ini.vals, fix.nug = TRUE, wei = "equal",
                 cov.model="cubic")


plot(vario)
lines(vfit)

kc <- krige.conv(vario.dat, loc = krig.grid, 
                 krige = krige.control(cov.pars = vfit$cov.pars))

image(kc)
points(vario.dat, add=T)

, max.dist=250/60