# prepare summary of daily weather, including Kirstenbosch conditions
rm(list=ls())
meta <- read.csv('data/csv_masters/location_meta.csv',as.is=T)
head(meta)

dw <- read.csv('data/csv_masters/2012daily.csv')
dw$dtRange <- dw$Tmax - dw$Tmin
dw$dt1406 <- dw$T14-dw$T06
dw$Tm1406 <- (dw$T14+dw$T06)/2
head(dw)
dim(dw)
dw <- dw[meta$use4ClimateSummaries[match(dw$siteID,meta$siteID)]==1,]
dim(dw)

dlySummary <- data.frame(year=2012,month=NA,day=NA,doy=1:366,date=as.Date(1:366,origin='2011-12-31'),Tmin=NA,Tmax=NA,Tmean=NA,dtRange=NA,T06=NA,T14=NA,Tm1406=NA,dt1406=NA,RHmin=NA,RHmax=NA,VPmax=NA,RHsat.hrs=NA,TminSr=NA,TmaxSr=NA,RHsatSr=NA,VPmaxSr=NA)
head(dlySummary)

dlySummary$month <- as.numeric(substr(dlySummary$date,6,7))
dlySummary$day <- as.numeric(substr(dlySummary$date,9,10))
head(dlySummary)
tail(dlySummary)

vars <- c('Tmin','Tmax','Tmean','dtRange','T06','T14','Tm1406','dt1406','RHmin','RHmax','VPmax','RHsat.hrs')

dw2 <- dw[,c('doy','siteID',vars)]
head(dw2)

plot(tapply(dw2$Tmin,dw2$doy,mean,na.rm=T))

i=1
for (i in 1:length(vars)) dlySummary[,(i+5)] <- tapply(dw2[,i+2],dw2$doy,mean,na.rm=T)
head(dlySummary)

# now calculate the spatial range across sites for each focal variable on each day
dlySummary$TminSr <- tapply(dw2$Tmin,dw2$doy,max,na.rm=T) - tapply(dw2$Tmin,dw2$doy,min,na.rm=T)
dlySummary$TmaxSr <- tapply(dw2$Tmax,dw2$doy,max,na.rm=T) - tapply(dw2$Tmax,dw2$doy,min,na.rm=T)
dlySummary$RHsatSr <- tapply(dw2$RHsat.hrs,dw2$doy,max,na.rm=T) - tapply(dw2$RHsat.hrs,dw2$doy,min,na.rm=T)
dlySummary$VPmaxSr <- tapply(dw2$VPmax,dw2$doy,max,na.rm=T) - tapply(dw2$VPmax,dw2$doy,min,na.rm=T)

pairs(dlySummary[,c('TminSr','TmaxSr','RHsatSr','VPmaxSr')])
plot(TminSr~Tmin,data=dlySummary)
plot(TmaxSr~Tmax,data=dlySummary)
plot(RHsatSr~RHsat.hrs,data=dlySummary)
plot(VPmaxSr~VPmax,data=dlySummary)

####
syn <- read.csv('data/TM_synoptics/soms.csv',as.is=T)
head(syn)
syn <- syn[order(syn$doy),]

dlySummary <- merge(dlySummary,syn[,c('doy','sommax','sommin')],all=T)
head(dlySummary)

## Now add Kirstenbosch data
require(circular)
weighted.circular.mean <- function(x,w,units='rad') {
    if (units=='deg' | units=='degree' | units=='degrees') {
        x <- 2*pi*x/360
    }
    if (is.null(w)) w <- rep(1,length(x))
    wms <- weighted.mean(sin(x),w)
    wmc <- weighted.mean(cos(x),w)
    wcm <- atan2(wms,wmc)
    if (wcm<0) wcm <- 2*pi+wcm
    if (units=='deg' | units=='degree' | units=='degrees') {
        wcm <- 360*wcm/(2*pi)
    }
    return(wcm)
}

Kps <- read.csv('data/Kbosch_weather_station_data/daily_csv/pressure_hPa.csv',as.is=T)
head(Kps)
exCol <- grep('X',names(Kps))
names(Kps)[exCol]
Kps <- Kps[,-exCol]
head(Kps)

Kwnd <- read.csv('data/Kbosch_weather_station_data/daily_csv/wind_deg_m.s.csv',as.is=T)
head(Kwnd)
exCol <- grep('X',names(Kwnd))
names(Kwnd)[exCol]
Kwnd <- Kwnd[,-exCol]
head(Kwnd)
Kwdir <- Kwnd
Kwdir <- Kwdir[,-(28:29)]
Kwspd <- Kwnd
Kwspd <- Kwspd[,-(28:29)]

h=4
for (h in 4:27) {
    k <- Kwnd[,h]
    k <- as.character(k)
    k[nchar(k)==1] <- '00000'
    k[nchar(k)==3] <- paste('00',k[nchar(k)==3],sep='')
    k[nchar(k)==4] <- paste('0',k[nchar(k)==4],sep='')
    #print(table(nchar(k))) # All cases accounted for
    Kwnd[,h] <- k
    
    Kwdir[,h] <- as.numeric(substr(Kwnd[,h],1,3))
    Kwspd[,h] <- as.numeric(substr(Kwnd[,h],4,5))
}

Kwspd$wind.speed.mean <- apply(Kwspd[,4:27],1,mean,na.rm=T)
Kwspd$wind.speed.max <- apply(Kwspd[,4:27],1,max,na.rm=T)
plot(Kwspd$DOY,Kwspd$wind.speed.max,type='l')

# Calculate weighted mean direction
plot(as.numeric(Kwdir[1,4:27]),as.numeric(Kwspd[1,4:27]))
wcm <- weighted.circular.mean(Kwdir[1,4:27],w=Kwspd[1,4:27],units='deg')
wcm

names(Kwdir)
Kwdir$wtd.mean.dir <- NA
for (d in 1:366) Kwdir$wtd.mean.dir[d] <- weighted.circular.mean(Kwdir[d,4:27],Kwspd[d,4:27],units='deg')
head(Kwdir)
plot(Kwdir$wtd.mean.dir,Kwspd$wind.speed.mean)
plot(Kwdir$DOY,Kwdir$wtd.mean.dir)

allDir <- c()
allSpd <- c()
for (h in 4:27) {
    allDir <- c(allDir,Kwdir[,h])
    allSpd <- c(allSpd,Kwspd[,h])
}
hist(allDir)
hist(allSpd)

Kppt <- read.csv('data/Kbosch_weather_station_data/daily_csv/precip_mm.csv')
head(Kppt)
exCol <- grep('X',names(Kppt))
names(Kppt)[exCol]
Kppt <- Kppt[,-exCol]
Kppt[is.na(Kppt)] <- 0
head(Kppt)

names(Kps)
mm <- range(c(Kps$avg,Kps$mx,Kps$mn))
plot(Kps$DOY,Kps$avg,ylim=mm,type='l')
points(Kps$DOY,Kps$mn,type='l',col='blue')
points(Kps$DOY,Kps$mx,type='l',col='red')

names(Kppt)
plot(Kppt$DOY,Kppt$tot,type='l')
plot(Kps$mn,Kppt$tot)

## add Kirstenbosch data to dlySummaries
head(dlySummary)
dlySummary$Kps.mean_hPa <- Kps$avg
dlySummary$Kppt_mm <- Kppt$tot
dlySummary$Kwspd.mean_m.s <- Kwspd$wind.speed.mean/10
dlySummary$Kwspd.max_m.s <- Kwspd$wind.speed.max/10
head(dlySummary)

write.csv(dlySummary,'data/csv_outfiles/dlySummary.csv',quote=F,row.names=F)
