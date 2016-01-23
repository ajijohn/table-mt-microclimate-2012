#extract topographic variables for each site
rm(list=ls())
library(sp)
library(rgeos)
library(raster)

meta = read.csv('/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/site.meta/LoggerSeries-2012.csv',comment.char="#",as.is=T)
head(meta);tail(meta)
loggerpoints <- SpatialPoints(data.frame(meta$UTM.east,meta$UTM.north))
projection(loggerpoints) <- CRS("+proj=utm +zone=34 +south")
plot(loggerpoints,axes=T)
lpoints.ll <- spTransform(loggerpoints,CRS('+proj=longlat +datum=WGS84'))
plot(lpoints.ll,axes=T)

# have a look at files created by this script back in 2012:
metadem <- read.csv('/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/site.meta/metadem.csv')
msol <- read.csv(file='/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/site.meta/monthly_solins.csv')
head(metadem)
head(msol)

# directory of dem10 layers
setwd('~');getwd()
setwd('Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/TMSM.10m.rasters/utm34/grd')
dir()
dem10 <- raster('dem10.grd')
plot(dem10)
plot(loggerpoints,add=T)
elevation <- extract(dem10,loggerpoints)

d2at <- raster('dist2atlantic.grd')
plot(d2at)
plot(loggerpoints,add=T)
d2at <- extract(d2at,loggerpoints)

d2fb <- raster('dist2falsebay.grd')
plot(d2fb)
d2fb <- extract(d2fb,loggerpoints)
d2cs <- raster('dist2coast.grd')
plot(d2cs)
d2cs <- extract(d2cs,loggerpoints)

ras <- raster('tmsm.domains.grd')
plot(ras)
plot(loggerpoints,add=T)
domain <- extract(ras,loggerpoints)
domain[is.na(domain)] <- 1
domain[meta$siteID=='E1.KRT'] <- NA
domain

slp10 <- raster('slp10.grd')
projection(slp10)
slope <- extract(slp10,loggerpoints)

ras <- raster('plow050.grd')
plot(ras)
plot(loggerpoints,add=T)
plow050 <- extract(ras,loggerpoints)
ras <- raster('plow125.grd')
plow125 <- extract(ras,loggerpoints)
ras <- raster('plow250.grd')
plow250 <- extract(ras,loggerpoints)
ras <- raster('plow500.grd')
plow500 <- extract(ras,loggerpoints)

ras <- raster('tpi050.grd')
tpi050 <- extract(ras,loggerpoints)
ras <- raster('tpi125.grd')
tpi125 <- extract(ras,loggerpoints)
ras <- raster('tpi250.grd')
tpi250 <- extract(ras,loggerpoints)
ras <- raster('tpi500.grd')
tpi500 <- extract(ras,loggerpoints)

ras <- raster('thl10.grd')
thl <- extract(ras,loggerpoints)

ras <- raster('rad_tot_080.grd')
rad080 <- extract(ras,loggerpoints)
ras <- raster('rad_tot_172.grd')
rad172 <- extract(ras,loggerpoints)
ras <- raster('rad_tot_355.grd')
rad355 <- extract(ras,loggerpoints)

topo10 <- data.frame(siteID=meta$siteID,domain,elevation,slope,d2at,d2fb,d2cs,rad080,rad172,rad355,thl,plow050,plow125,plow250,plow500,tpi050,tpi125,tpi250,tpi500)

head(topo10)
topo10[,1:15]
write.csv(topo10,'/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/site.meta/topo10.csv',row.names=F)

## now repeat for 30m dem
setwd('~');getwd()
setwd('/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem30/topography/arc_grids')
dir()
dem <- raster('dem30.grd')
plot(dem)
plot(lpoints.ll,add=T)
elevation <- extract(dem,lpoints.ll)

slp30 <- raster('slp30.grd')
projection(slp30)
slope <- extract(slp30,lpoints.ll)

ras <- raster('plow050.grd')
plot(ras)
plot(lpoints.ll,add=T)
plow050 <- extract(ras,lpoints.ll)
ras <- raster('plow125.grd')
plow125 <- extract(ras,lpoints.ll)
ras <- raster('plow250.grd')
plow250 <- extract(ras,lpoints.ll)
ras <- raster('plow500.grd')
plow500 <- extract(ras,lpoints.ll)

ras <- raster('tpi050.grd')
tpi050 <- extract(ras,lpoints.ll)
ras <- raster('tpi125.grd')
tpi125 <- extract(ras,lpoints.ll)
ras <- raster('tpi250.grd')
tpi250 <- extract(ras,lpoints.ll)
ras <- raster('tpi500.grd')
tpi500 <- extract(ras,lpoints.ll)

ras <- raster('thl30.grd')
thl <- extract(ras,loggerpoints)

revsigny <- function(ras) {
    xras <- extent(ras)
    xras@ymin <- 10000000+xras@ymin
    xras@ymax <- 10000000+xras@ymax
    ras <- setExtent(ras,xras)
    return(ras)
}

setwd('/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem30/TMSM.30m.rasters/utm34')
ras <- revsigny(raster('rad_tot_080.grd'))
plot(ras)
plot(loggerpoints,add=T)
rad080 <- extract(ras,loggerpoints)
ras <- revsigny(raster('rad_tot_172.grd'))
rad172 <- extract(ras,loggerpoints)
ras <- revsigny(raster('rad_tot_355.grd'))
rad355 <- extract(ras,loggerpoints)

topo30 <- data.frame(siteID=meta$siteID,domain=topo10$domain,elevation,slope,d2at=topo10$d2at,d2fb=topo10$d2fb,d2cs=topo10$d2cs,rad080,rad172,rad355,thl,plow050,plow125,plow250,plow500,tpi050,tpi125,tpi250,tpi500)

head(topo30)
topo30[,1:15]
topo30[,c(1,16:19)]
write.csv(topo30,'/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/site.meta/topo30.csv',row.names=F)

