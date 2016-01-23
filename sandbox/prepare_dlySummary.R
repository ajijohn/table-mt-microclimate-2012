
meta <- read.csv('data/csv_masters/location_meta.csv',as.is=T)
head(meta)

dw <- read.csv('data/csv_masters/2012daily.csv')
dw$dtRange <- dw$Tmax - dw$Tmin
head(dw)
dim(dw)
dw <- dw[meta$use4ClimateSummaries[match(dw$siteID,meta$siteID)]==1,]
dim(dw)

dlySummary <- data.frame(year=2012,doy=1:366,date=as.Date(1:366,origin='2011-12-31'),Tmin=NA,Tmax=NA,Tmean=NA,dtRange=NA,RHmin=NA,RHmax=NA,VPmax=NA,RHsat.hrs=NA)

head(dlySummary)

vars <- c('Tmin','Tmax','Tmean','dtRange','RHmin','RHmax','VPmax','RHsat.hrs')

dw2 <- dw[,c('doy','siteID',vars)]
head(dw2)

plot(tapply(dw2$Tmin,dw2$doy,mean,na.rm=T))

i=1
for (i in 1:8) dlySummary[,(i+3)] <- tapply(dw2[,i+2],dw2$doy,mean,na.rm=T)
head(dlySummary)

write.csv(dlySummary,'data/csv_outfiles/dlySummary.csv',quote=F,row.names=F)
