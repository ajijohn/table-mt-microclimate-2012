## script to calculate TPI and plow at different scales. I did this previously but don't have record. 
## 2015-06-20
rm(list=ls())
require(raster)
require(sp)
require(rgeos)
source('/Users/david/Documents/Projects/Toolbox/spatial_tools/topo.position.R')

# start with 30m
dem30 <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem30/topography/arc_grids/dem30.grd')
projection(dem30)
#plot(dem30)

#dem <- crop(dem30,drawExtent())
dem <- dem30
plot(dem)

## TESTING
out='tpi'
rad=500
na.rm=F
dbug=T

plot(dem30)
dem <- crop(dem30,drawExtent())
dem
tpi500 <- topo.position(dem,'tpi',rad=500)
plot(tpi500)
tpi500

plow500 <- topo.position(dem,'pr.low',rad=500)
plot(plow500)
plow500

tpi400 <- topo.position(dem,'tpi',rad=400)
plot(tpi400)
tpi400

tpi50 <- topo.position(dem,'tpi',rad=50)
plot(tpi50)
tpi50
t50v <- getValues(tpi50)
hist(t50v)

#old50 <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem30/topography/arc_grids/tpi50.grd')
#plot(old50)

plot(getValues(tpi50),getValues(tpi500))
plot(getValues(tpi50),getValues(dem),xlim=c(-30,30))
plot(getValues(tpi500),getValues(dem))
## END TESTING

# now make and save layers
tpi50 <- topo.position(dem,'tpi',rad=50)
writeRaster(tpi50,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem30/topography/arc_grids/tpi050.grd',overwrite=T)

tpi125 <- topo.position(dem,'tpi',rad=125)
writeRaster(tpi125,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem30/topography/arc_grids/tpi125.grd',overwrite=T)

tpi250 <- topo.position(dem,'tpi',rad=250)
writeRaster(tpi250,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem30/topography/arc_grids/tpi250.grd',overwrite=T)

tpi500 <- topo.position(dem,'tpi',rad=500)
writeRaster(tpi500,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem30/topography/arc_grids/tpi500.grd',overwrite=T)

plow50 <- topo.position(dem,'pr.low',rad=50)
writeRaster(plow50,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem30/topography/arc_grids/plow050.grd',overwrite=T)

plow125 <- topo.position(dem,'pr.low',rad=125)
writeRaster(plow125,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem30/topography/arc_grids/plow125.grd',overwrite=T)

plow250 <- topo.position(dem,'pr.low',rad=250)
writeRaster(tpi250,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem30/topography/arc_grids/plow250.grd',overwrite=T)

plow500 <- topo.position(dem,'pr.low',rad=500)
writeRaster(plow500,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem30/topography/arc_grids/plow500.grd',overwrite=T)

plot(stack(tpi50,tpi125,tpi250,tpi500))
plot(stack(plow50,plow125,plow250,plow500))

length(tpi50)
rsamp <- sample(length(tpi50),1e4)
allV <- getValues(stack(dem,tpi50,tpi125,tpi250,tpi500,plow50,plow125,plow250,plow500))
summary(allV)
allVT <- subset(allV,allV[,1]!=0)
head(allVT)
dim(allVT)
pairs(allVT[sample(nrow(allVT),1e4),],pch='.')
print(round(cor(allVT),3))

# now with 10m dem

# used this to manually crop from full Cape Pen DEM, result read in below
# dem10 <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/CPdem10/dem10m.asc')
# plot(dem10)
# proj4string(dem10) <- CRS('+proj=utm +zone=34')
# 
# dem <- crop(dem10,drawExtent())
# plot(dem)
# writeRaster(dem,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/TMSM.10m.rasters/utm34/grd/dem10_TMSM.grd',overwrite=T)

dem <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/TMSM.10m.rasters/utm34/grd/dem10.grd')
projection(dem)
plot(dem)
res(dem)
dem10N <- dem

meta = read.csv('/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/site.meta/LoggerSeries-2012.csv',comment.char="#",as.is=T)
loggerpoints <- SpatialPoints(data.frame(meta$UTM.east,meta$UTM.north))
projection(loggerpoints) <- CRS("+proj=utm +zone=34 +south")
plot(loggerpoints,add=T)

## TESTING
plow50 <- topo.position(dem10N,'pr.low',rad=50)
plot(plow50)

plow50.30 <- topo.position(dem30,'pr.low',rad=50)
plot(plow50.30)

old125 <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/TMSM.10m.rasters/utm34/grd/tpi125.grd')
plot(old125)
old125
tpi125 <- topo.position(dem10N,'tpi',rad=125)
plot(tpi125)
tpi125

oldp500 <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/TMSM.10m.rasters/utm34/grd/plow500.grd')
plot(oldp500)
plow50 <- topo.position(dem,'pr.low',rad=50)
plot(plow500)
## END TESTING

# now make and save layers
tpi50 <- topo.position(dem10N,'tpi',rad=50)
writeRaster(tpi50,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/TMSM.10m.rasters/utm34/grd/tpi050.grd',overwrite=T)

tpi125 <- topo.position(dem,'tpi',rad=125)
writeRaster(tpi125,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/TMSM.10m.rasters/utm34/grd/tpi125.grd',overwrite=T)

tpi250 <- topo.position(dem,'tpi',rad=250)
writeRaster(tpi250,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/TMSM.10m.rasters/utm34/grd/tpi250.grd',overwrite=T)

tpi500 <- topo.position(dem,'tpi',rad=500)
writeRaster(tpi500,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/TMSM.10m.rasters/utm34/grd/tpi500.grd',overwrite=T)

plow50 <- topo.position(dem,'pr.low',rad=50)
writeRaster(plow50,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/TMSM.10m.rasters/utm34/grd/plow050.grd',overwrite=T)

plow125 <- topo.position(dem,'pr.low',rad=125)
writeRaster(plow125,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/TMSM.10m.rasters/utm34/grd/plow125.grd',overwrite=T)

plow250 <- topo.position(dem,'pr.low',rad=250)
writeRaster(plow250,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/TMSM.10m.rasters/utm34/grd/plow250.grd',overwrite=T)

plow500 <- topo.position(dem,'pr.low',rad=500)
writeRaster(plow500,'/Users/david/Documents/Projects/SouthAfricaResearch/CFRdata/CapePen/dem10/TMSM.10m.rasters/utm34/grd/plow500.grd',overwrite=T)

plot(stack(tpi50,tpi125,tpi250,tpi500))
plot(stack(plow50,plow125,plow250,plow500))

allV <- getValues(stack(dem,tpi50,tpi125,tpi250,tpi500,plow50,plow125,plow250,plow500))
dim(allV)
summary(allV)
allVT <- subset(allV,allV[,1]!=0)
head(allVT)
dim(allVT)
pairs(allVT[sample(nrow(allVT),1e4),],pch='.')
print(round(cor(allVT),3))
