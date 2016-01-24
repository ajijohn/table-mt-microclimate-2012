## script with summary statistics used in manuscript
rm(list=ls())
library(random)

# table of metadata for each location
meta <- read.csv(file='data/csv_masters/location_meta.csv',as.is=T)
head(meta)
dim(meta)

#exclude race track and get range of site elevations
krt <- which(meta$siteID=='E1.KRT')
summary(meta$elevation[-krt])

# load list of 366 daily climate data.frames
cbday <- readRDS(file='data/Rdata/cbday.Rdata')
class(cbday)
length(cbday)

# look at structure of each data.frame in list
class(cbday[[1]])
head(cbday[[1]])
dim(cbday[[1]])

# check daily climate records and calculate completeness
#8WYM1i3fkuO255m4npfy
all(cbday[[1]]$siteID %in% meta$siteID)

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
# When these results are reported in the paper, values were reduced by 1 because the Porcupine site was missing for all of 2012, and shouldn't be counted. It's in data files for completeness.
# END missing sites calculation by day

# read in topographic variables for each site, based on 10 m dem
topo10 <- read.csv('data/csv_masters/topo10.csv')
head(topo10)
names(topo10)


# check range of values and pairwise correlations
means <- apply(topo10[,-1],2,mean,na.rm=T)
mins <- apply(topo10[,-1],2,min,na.rm=T)
maxs <- apply(topo10[,-1],2,max,na.rm=T)
tmmm <- cbind(means,mins,maxs)
names(means)

# EXTRACT STATS FOR TABLE 1
#h5EQIIeZ3DeMkpPgtHjM
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
round(tmmm[17,],1) #tpi250
round(tmmm[18,],1) #tpi500
plot(topo10$plow125,topo10$tpi125)
## END TABLE 1 STATS

# TABLE S2 pairwise correlations between topographic variables
#O5WhKLtJxNOi2oCHWcUn
pcor <- cor(topo10[,-c(1:2)],use='pair')
write.csv(pcor,'data/results/TableS2-paircorr-topo10.csv',quote=F,row.names=F)
#

## Check completeness by site
cbsite <- readRDS('data/Rdata/cbsite.Rdata')
tmin.missing <- c()
tmax.missing <- c()
RHsat.missing <- c()
i=1
for (i in 1:length(cbsite)) {
    tmin.missing[i] <- length(which(is.na(cbsite[[i]]$Tmin)))
    tmax.missing[i] <- length(which(is.na(cbsite[[i]]$Tmax)))
    RHsat.missing[i] <- length(which(is.na(cbsite[[i]]$RHsat.hrs)))
}
print(tmin.missing)
print(tmax.missing)
print(RHsat.missing)
days_missing_by_site <- data.frame(siteID=names(cbsite),tmin.missing,tmax.missing,RHsat.missing)
pairs(days_missing_by_site[,-1])
##

## Table S1
#ci3hxIsv8TymnEOlRw7F
# Critical information about locations
meta <- read.csv(file='data/csv_masters/location_meta.csv',as.is=T)
head(meta)

# check site order is the same, and add missing data to meta dataframe
all(meta$siteID==days_missing_by_site$siteID)
meta <- data.frame(meta,tmin.missing,tmax.missing,RHsat.missing)

#relabel domain for Kenilworth Race Track as NA (not in Silvermine or Table Mt domains)
meta$domain[grep('E1.KRT',meta$siteID)] <- NA

# make dataframe for writing out table
metaOut <- meta

# drop site that was moved in 2011, and is all missing values
metaOut <- metaOut[-which(tmin.missing==366),]

# reorder rows for output
siteOrder <- order(metaOut$domain,metaOut$siteID)
metaOut <- metaOut[siteOrder,]
head(metaOut)
tail(metaOut)
names(metaOut)
write.csv(metaOut[,c('domain','siteID','logger','UTM.east','UTM.north','elevation')],'data/results/TableS1-site-info.csv',row.names=F)

## END TABLE S1

topo30 <- read.csv('data/csv_masters/topo30.csv')
head(topo30)

dlySummary <- read.csv('/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/Rdata.files/dlySummary.csv',as.is=T)
head(dlySummary)
    
# RESULTS
ymd <- strsplit(dlySummary$date,'-')
ymd <- matrix(unlist(ymd),366,3,byrow=T)
head(ymd)
dlySummary$year <- 2012
dlySummary$month <- as.numeric(ymd[,2])
dlySummary$day <- as.numeric(ymd[,3])
