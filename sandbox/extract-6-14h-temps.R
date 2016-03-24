## extract 6am and 2pm temperatures to see if lapse rates are more clearcut for simultaneous measurements

# Choose single time that is close to min and max each day
cd <- read.csv('data/csv_masters/2012daily.csv',as.is=T)
head(cd)
dim(cd)
hist(cd$Tmin.time,breaks=-10:16)
mean(cd$Tmin.time,na.rm=T)
# mode is at 6am, though mean is earlier due to skew towards night. Going with 6am

# max times has values that spilled into next day, fixed below
hist(cd$Tmax.time)
cd$Tmax.time[which(cd$Tmax.time>24)] <- cd$Tmax.time[which(cd$Tmax.time>24)]-24
hist(cd$Tmax.time,breaks=0:24)
mean(cd$Tmax.time,na.rm=T)
# model is 13-1500h, so going with 1400

ddir <- '/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/csv/concatenated/'
dfiles <- dir(ddir)
length(dfiles)

t0600 <- matrix(NA,nrow=length(dfiles),ncol=366)
t1400 <- matrix(NA,nrow=length(dfiles),ncol=366)

i=1
for (i in 1:length(dfiles)) {
    print(i)
    dd <- read.csv(paste(ddir,dfiles[i],sep=''))    
    head(dd)
    dd <- dd[which(dd$year==2012),]
    head(dd)
    tail(dd)
    l6 <- which(dd$t24==6.0)
    if (length(l6)==366) {
        t0600[i,] <- dd$TempC[l6]
    } else {
        for (d in 1:366) {
            dr <- which(dd$doy11==d+365)
            if (length(dr)>0) {
                ddd <- dd[dr,]
                d6 <- abs(ddd$t24-6)
                rc <- which.min(d6)
                if (d6[rc]<0.5) t0600[i,d] <- ddd$TempC[rc]
            }
        }
    }
    l14 <- which(dd$t24==14)
    if (length(l14)==366) {
        t1400[i,] <- dd$TempC[l14]
    } else {
        for (d in 1:366) {
            dr <- which(dd$doy11==d+365)
            if (length(dr)>0) {
                ddd <- dd[dr,]
                d14 <- abs(ddd$t24-14)
                rc <- which.min(d14)
                if (d14[rc]<0.5) t1400[i,d] <- ddd$TempC[rc]
            }
        }
    }
}

dim(t0600)
plot(apply(t0600,1,mean,na.rm=T),apply(t1400,1,mean,na.rm=T))
plot(apply(t0600,2,mean,na.rm=T))
plot(apply(t1400,2,mean,na.rm=T))
plot(apply(t0600,2,mean,na.rm=T),apply(t1400,2,mean,na.rm=T))

head(dfiles)
dlog <- substr(dfiles,1,nchar(dfiles)-4)
head(dlog)

meta <- read.csv('data/csv_masters/location_meta.csv',as.is=T)
head(meta)
rorder <- match(meta$logger,dlog)

t0600b <- t0600[rorder,]
t1400b <- t1400[rorder,]

plot(meta$elevation,apply(t0600b,1,mean,na.rm=T))
plot(meta$elevation,apply(t1400b,1,mean,na.rm=T))

t0600vec <- as.numeric(t0600b)
t1400vec <- as.numeric(t1400b)

plot(cd$Tmin,t0600vec)
plot(cd$Tmax,t1400vec)

cd$T06 <- t0600vec
cd$T14 <- t1400vec

head(cd)
write.csv(cd,'data/csv_masters/2012daily.csv',row.names=F)
