## Models for each day of 2012
## Rewriting script 2015-06-18 to use data.frames by variable

#----------------#
# LOAD LIBRARIES #
#----------------#

rm(list=ls())
library(raster)
library(RColorBrewer)

#----------------#
# LOAD DATA      #
#----------------#
meta <- read.csv('data/csv_masters/location_meta.csv',as.is=T)
head(meta)

topo10 <- read.csv('data/csv_masters/topo10.csv',as.is=T)
head(topo10)

dlySol <- readRDS('data/Rdata/dlySol.Rdata')
head(dlySol)

source('~/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/analysis/TableMt_ms_analyses/analysis/insolation_functions.R')

d2r <- function(d) pi * d/180

#-----------------------#
# LOAD DLY WEATHER DATA #
#-----------------------#

cbday <- readRDS('data/Rdata/cbday.Rdata')
length(cbday)
head(cbday[[1]])
dim(cbday[[1]])

#--------------------------------#
# MAKE 3 CHAR DAYS VAR           #
#--------------------------------#
days <- as.character(1:366)
days[nchar(days)==1] <- paste('00',days[nchar(days)==1],sep='')
days[nchar(days)==2] <- paste('0',days[nchar(days)==2],sep='')

Tmax <- cbday[[90]]$Tmax
Tmax[meta$siteID=='E1.KRT'] <- NA

da <- c()
for (i in 1:nrow(topo10)) da[i] <- cos(divang(topo10$slope[i],topo10$aspect[i],d2r(30),d2r(180),unit='rad'))

dat <- data.frame(Tmax,elevation=topo10$elevation,cda=da,slope=topo10$slope,aspect=topo10$aspect)
dat <- dat[complete.cases(dat),]

plot(dat$aspect,dat$cda)

plot(Tmax~elevation,data=dat)
fit1 <- lm(Tmax~elevation,data=dat)
summary(fit1)
plot(dat$cda,fit1$residuals)

fit2 <- lm(Tmax~elevation+cda,data=dat)
anova(fit1,fit2)
summary(fit2)


Tmax <- cbday[[1]]$Tmax
Tmax[meta$siteID=='E1.KRT'] <- NA

aspects <- seq(0,360,by=10)
slopes <- seq(0,90,by=10)

results <- matrix(NA,ncol=4,nrow=length(slopes)*length(aspects))
i=1;j=1;k=0
for (i in 1:length(slopes)) for (j in 1:length(aspects)) {
    k <- k+1
    da <- c()
    for (m in 1:nrow(topo10)) da[m] <- cos(divang(topo10$slope[m],topo10$aspect[m],d2r(slopes[i]),d2r(aspects[j]),unit='rad'))
    
    dat <- data.frame(Tmax,elevation=topo10$elevation,cda=da,slope=topo10$slope,aspect=topo10$aspect)
    dat <- dat[complete.cases(dat),]
    
    fit2 <- lm(Tmax~elevation+cda,data=dat)
    results[k,1] <- slopes[i]
    results[k,2] <- aspects[j]
    results[k,3] <- summary(fit2)$r.sq
    results[k,4] <- fit2$coefficients[3]
}
results[which.max(results[,3]),]
plot(results[,c(2,1)],col=rev(heat.colors(20))[cut(results[,4],20)],pch=19)
summary(results[,4])
