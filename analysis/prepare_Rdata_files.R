## read in all the daily csvs and make an Rdata list object for analysis
rm(list=ls())
cd <- read.csv('data/csv_masters/2012daily.csv')

cbday <- list()
i=1
for (i in 1:366) {
    dw <- subset(cd,cd$doy==i)
    dim(dw)
    head(dw)
    cbday[[i]] <- dw
}

days <- as.character(1:366)
days[which(nchar(days)<3)] <- paste('0',days[which(nchar(days)<3)],sep='')
days[which(nchar(days)<3)] <- paste('0',days[which(nchar(days)<3)],sep='')

names(cbday) <- paste('doy',days,sep='')
length(cbday)
names(cbday)
head(cbday[['doy001']])
saveRDS(cbday,'data/Rdata/cbday.Rdata')
