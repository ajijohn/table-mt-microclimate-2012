## script to prepare csv files for sharing with publication

# site metadata
load('/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/Rdata.files/meta_out.Rdata')
head(meta)

# calulate lat/long values
library(sp)
library(rgeos)
library(rgdal)
msp <- SpatialPoints(meta[,c('UTM.east','UTM.north')])
plot(msp)
proj4string(msp) <- CRS('+proj=utm +zone=34 +south')
plot(msp,axes=T)

mspLL <- spTransform(msp,CRS('+proj=longlat +datum=WGS84'))
plot(mspLL,axes=T)
LLc <- coordinates(mspLL)
head(LLc)
meta$LAT_DD <- LLc[,2]
meta$LON_DD <- LLc[,1]

head(meta)
usecol <- c(1:24,35:37)
names(meta)[usecol]

meta2 <- meta[,usecol]
names(meta2)
colOrder <- c(13,4,5,17,16,24,23,18,19,20,21,26,27)
addCol <- which(!(1:27 %in% colOrder))
colOrder <- c(colOrder,addCol)
colOrder
names(meta2)[colOrder]

write.csv(meta2[,colOrder],'data/csv_masters/location_meta.csv',row.names=F)
