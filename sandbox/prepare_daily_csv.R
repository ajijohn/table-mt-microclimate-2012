## prepare master csv files with daily weather for each station
#### This section prepares a single file, which is better for database format as it's the same structure across all days
rm(list=ls())
getwd()
dir()
meta <- read.csv('data/csv_masters/location_meta.csv')
head(meta)
dim(meta)
table(meta$logger)==2
diff(meta$logger)
meta[86:87,3]==9850229

# load('/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/Rdata.files/cbday.Rdata')
# length(cbday)
# cbday[[1]]$siteID==meta$siteID
# head(cbday[[1]])
# 
# days <- as.character(1:366)
# days[which(nchar(days)<3)] <- paste('0',days[which(nchar(days)<3)],sep='')
# days[which(nchar(days)<3)] <- paste('0',days[which(nchar(days)<3)],sep='')
# days                                          
# 
# fname <- paste('data/csv_masters/2012daily.csv',sep='')
# cd <- cbday[[1]]
# cd$doy <- 1
# head(cd)
# 
# i=2
# for (i in 2:366) {
#     tmp <- cbday[[i]]
#     tmp$doy <- i
#     cd <- rbind(cd,tmp)
# }
# dim(cd)
# tail(cd)
# write.csv(cd,fname,row.names=F)

# start here to read in master file and prepare Rdata objects
cd <- read.csv('data/csv_masters/2012daily.csv')
dim(cd)
head(cd)
all(table(cd$siteID)==366)
all(table(cd$doy==96))

cbday <- list()
for (i in 1:366) cbday[[i]] <- subset(cd,cd$doy==i)

days <- as.character(1:366)
days[which(nchar(days)<3)] <- paste('0',days[which(nchar(days)<3)],sep='')
days[which(nchar(days)<3)] <- paste('0',days[which(nchar(days)<3)],sep='')
names(cbday) <- paste('doy',days,sep='')                                       
saveRDS(cbday,'data/Rdata/cbday.Rdata')

cbsite <- list()
siteNames <- as.character(unique(cd$siteID))
for (i in 1:96) cbsite[[i]] <- subset(cd,cd$siteID==siteNames[i])
names(cbsite) <- siteNames
saveRDS(cbsite,'data/Rdata/cbsite.Rdata')
