## script with summary statistics used in manuscript

rm(list=ls())
meta <- read.csv(file='data/csv_masters/location_meta.csv')
head(meta)

#exclude race track and get range of site elevations
krt <- which(meta$siteID=='E1.KRT')
summary(meta$elevation[-krt])

# check daily climate records and calculate completeness
cbday <- readRDS(file='data/Rdata/cbday.Rdata')
length(cbday)
class(cbday[[1]])
head(cbday[[1]])

tmin.missing <- 0
tmax.missing <- 0
vpmax.missing <- 0
rhsat.missing <- 0

tmin.dmax.missing <- 0
tmax.dmax.missing <- 0
vpmax.dmax.missing <- 0
rhsat.dmax.missing <- 0

for (i in 1:366) {
    tmp <- length(which(is.na(cbday[[i]]$Tmin)))
    tmin.missing <- tmin.missing + tmp
    if (tmp > tmin.dmax.missing) tmin.dmax.missing <- tmp
    
    tmp <- length(which(is.na(cbday[[i]]$Tmax)))
    tmax.missing <- tmax.missing + tmp
    if (tmp > tmax.dmax.missing) tmax.dmax.missing <- tmp
    
    tmp <- length(which(is.na(cbday[[i]]$VPmax)))
    vpmax.missing <- vpmax.missing + tmp
    if (tmp > vpmax.dmax.missing) vpmax.dmax.missing <- tmp
    
    tmp <- length(which(is.na(cbday[[i]]$RHsat.hrs)))
    rhsat.missing <- rhsat.missing + tmp
    if (tmp > rhsat.dmax.missing) rhsat.dmax.missing <- tmp
    
}

# total number of possible records:
ntot <- 366*96 #35136

# % percent missing
tmin.missing/ntot
tmax.missing/ntot
vpmax.missing/ntot
rhsat.missing/ntot

tmin.dmax.missing
tmax.dmax.missing
vpmax.dmax.missing
rhsat.dmax.missing

# check range of values and pairwise correlations
topo10 <- read.csv('data/csv_masters/topo10.csv')
head(topo10)
names(topo10)
means <- apply(topo10[,-1],2,mean,na.rm=T)
mins <- apply(topo10[,-1],2,min,na.rm=T)
maxs <- apply(topo10[,-1],2,max,na.rm=T)
tmmm <- cbind(means,mins,maxs)
names(means)

# EXTRACT STATS FOR TABLE 1
round(tmmm[2,],0) #elevation
round(tmmm[3,],3) #slope
round(tmmm[4,],0) #distance to atlantic
round(tmmm[5,],0) #distnace to false bay
round(tmmm[6,],0) #distance to ocean
round(tmmm[7,],0) #rad080 (equinox)
round(tmmm[8,],0) #rad172 (winter solstice)
round(tmmm[9,],0) #rad355 (summer solstice)
round(tmmm[10,],3) #THL (units??)
round(tmmm[11,],3) #plow50
round(tmmm[12,],3) #plow125
round(tmmm[13,],3) #plow250
round(tmmm[14,],3) #plow500
#plot(topo10$plow125,topo10$plow500)

round(tmmm[15,],1) #tpi50
round(tmmm[16,],1) #tpi125
round(tmmm[17,],1) #tpi250 - missing - this is TPI in data!
round(tmmm[18,],1) #tpi500
plot(topo10$plow125,topo10$tpi125)

## END TABLE 1 STATS

# Table S2 pairwise correlations
pcor <- cor(topo10[,-c(1:2)],use='pair')
write.csv(pcor,'data/results/paircorr-topo10.csv',quote=F,row.names=F)

topo30 <- read.csv('data/csv_masters/topo30.csv')
head(topo30)

dlySummary <- read.csv('/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/Rdata.files/dlySummary.csv')
head(dlySummary)
    