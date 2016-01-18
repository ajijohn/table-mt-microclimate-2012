## prepare master csv files with daily weather for each station
#### This section prepares a single file, which is better for database format as it's the same structure across all days
rm(list=ls())
getwd()
dir()
meta <- read.csv('data/csv_masters/')
head(meta)
dim(meta)
table(meta$logger)==2
diff(meta$logger)
meta[86:87,3]==9850229

load('/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/Rdata.files/cbday.Rdata')
length(cbday)
cbday[[1]]$siteID==meta$siteID
head(cbday[[1]])

days <- as.character(1:366)
days[which(nchar(days)<3)] <- paste('0',days[which(nchar(days)<3)],sep='')
days[which(nchar(days)<3)] <- paste('0',days[which(nchar(days)<3)],sep='')
days                                          

fname <- paste('data/csv_masters/2012daily.csv',sep='')
cd <- cbday[[1]]
cd$doy <- 1
head(cd)

i=2
for (i in 2:366) {
    tmp <- cbday[[i]]
    tmp$doy <- i
    cd <- rbind(cd,tmp)
}
dim(cd)
tail(cd)
write.csv(cd,fname,row.names=F)
