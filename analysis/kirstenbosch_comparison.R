## compare daily min-max at Kirstenbosch from weather station and hobo
# create dataframe with daily records for kirstenbosch
rm(list=ls())
cd <- read.csv('data/csv_masters/2012daily.csv')
dim(cd)
kbg <- which(cd$siteID=='E2.KBG')
cbd.kbg <- cd[kbg,]
dim(cbd.kbg)

# now read in records from kirstenbosch weather station
# these records are not public and are not on the github repository for this project
wst.kbg <- read.csv('/Users/david/Documents/Projects/TableMtProject/Kbosch_weather_station_data/daily_csv/temperature_dC.csv',as.is=T)
head(wst.kbg)
dim(wst.kbg)
wst.kbg$doy <- 1:366

plot(wst.kbg$doy,wst.kbg$max,type='l')
points(cbd.kbg$Tmax,type='l',col='blue')

plot(wst.kbg$max,cbd.kbg$Tmax)
hist(wst.kbg$max-cbd.kbg$Tmax)

plot(wst.kbg$doy,wst.kbg$min,type='l')
points(cbd.kbg$Tmin,type='l',col='blue')

plot(wst.kbg$min,cbd.kbg$Tmin)
hist(wst.kbg$min-cbd.kbg$Tmin)
