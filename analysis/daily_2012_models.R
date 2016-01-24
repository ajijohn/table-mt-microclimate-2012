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

topo30 <- read.csv('data/csv_masters/topo30.csv',as.is=T)
head(topo30)

dw <- read.csv('data/csv_masters/2012daily.csv',as.is=T)
head(dw)

dlySummary <- read.csv('data/csv_outfiles/dlySummary.csv',as.is=T)
head(dlySummary)

dlySol <- readRDS('data/Rdata/dlySol.Rdata')
head(dlySol)

# merge SOM node numbers for min and max temp with dlySummary data
dlySummary <- merge(dlySummary,syn[,c('doy','sommax','sommin')],all=T)
head(dlySummary)
tail(dlySummary)
boxplot(Tmax~sommax,data=dlySummary)

#-------------------------------------------#
# CHECK CORRELATIONS IN TOPO VARIABLES      #
#-------------------------------------------#
# strong correlations that should be avoided in models:
cor(topo10[,-c(1:2)],use='pair')
pairs(topo10[,-c(1:2)])
names(topo10)

## find correlations > 0.5
i <- 3; j <- 4;
for (i in 3:(ncol(topo10)-1)) for (j in (i+1):ncol(topo10)) {
    rr <- cor(topo10[,i],topo10[,j],use='pair')
    if (abs(rr)>0.7) print(c(names(topo10)[c(i,j)],round(rr,3)))
}

# distance from Atlantic and False Bay, borderline
# Ran some models swapping these two variables in and out, and found false bay on average increased R2 more, and overall model R2 are significantly correlated so when one works, either works overall. Based on this, all models use Distance to False Bay.
cor(topo10[,c('d2at','d2fb')],use='pair') # R = -0.65

# equinox and winter solstice radiation
cor(topo10[,c('rad080','rad172')],use='pair') # R = 0.95

# all tpi variables with each other across scales, and plow with each other across scales; elevation weakly correlated with tpi and plow
cor(topo10[,c('elevation','tpi050','tpi125','tpi250','tpi500','plow050','plow125','plow250','plow500')],use='pair') # R >= 0.89
cor(topo30[,c('elevation','tpi050','tpi125','tpi250','tpi500','plow050','plow125','plow250','plow500')],use='pair') # R >= 0.89

# How does daily solar radiation compare to topographic heat load
# correlation max at 0.7 on day 48 (Feb 17) and around day 295 (Oct 21)
# minimum mid-summer solstice
solVthl <- c()
i=1
for (i in 1:366) solVthl[i] <- cor(topo10$thl,dlySol[,i],use='pair')
plot(1:366,solVthl)   
which.max(solVthl)

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

#-----------------------#
# TMAX MODELS           #
#-----------------------#
TmaxMods <- data.frame(doy=1:366,Tmax.m1.R2=NA,Tmax.m1.residsd=NA,Tmax.m1.elev.slope=NA,Tmax.m2.R2=NA,Tmax.m2.residsd=NA,Tmax.elev.slope=NA,Tmax.dsol.slope=NA,Tmax.plow500.slope=NA,Tmax.d2fb.slope=NA,Tmax.elev.pval=NA,Tmax.dsol.pval=NA,Tmax.plow500.pval=NA,Tmax.d2fb.pval=NA)
dim(TmaxMods)

# Initial tests used to select variables that perform best across the season
d=1
for (d in 1:366) {
    if (d %% 10 == 0) print(d)
    dd <- subset(dw,dw$doy==d)
    dd <- dd[match(dd$siteID,meta$siteID),]
    dd$Tmax[meta$use4Analyses==0] <- NA
    topo10$dsol <- dlySol[,paste('rad_tot_',days[d],sep='')]
    
    fit1 <- lm(dd$Tmax~elevation,data=topo10)
    TmaxMods$Tmax.m1.R2[d] <- summary(fit1)$r.s
    TmaxMods$Tmax.m1.residsd[d] <- sd(fit1$residuals)
    TmaxMods$Tmax.m1.elev.slope[d] <- fit1$coefficients[2]
    
    fit1 <- lm(dd$Tmax~elevation+dsol+plow500+d2fb,data=topo10)
    TmaxMods$Tmax.m2.R2[d] <- summary(fit1)$r.s
    TmaxMods$Tmax.m2.residsd[d] <- sd(fit1$residuals)
    TmaxMods$Tmax.elev.slope[d] <- fit1$coefficients[2]
    TmaxMods$Tmax.dsol.slope[d] <- fit1$coefficients[3]
    TmaxMods$Tmax.plow500.slope[d] <- fit1$coefficients[4]
    TmaxMods$Tmax.d2fb.slope[d] <- fit1$coefficients[5]
    TmaxMods$Tmax.elev.pval[d] <- anova(fit1)$P[1]
    TmaxMods$Tmax.dsol.pval[d] <- anova(fit1)$P[2]
    TmaxMods$Tmax.plow500.pval[d] <- anova(fit1)$P[3]
    TmaxMods$Tmax.d2fb.pval[d] <- anova(fit1)$P[4]
}
TmaxMods <- merge(TmaxMods,syn[,c('doy','sommax')],all=T)
head(TmaxMods)
tail(TmaxMods)

table(TmaxMods$sommax)
plot(Tmax.m1.R2~doy,data=TmaxMods,type='l')
plot(Tmax.m2.R2~doy,data=TmaxMods,type='l')
plot(I(Tmax.m2.R2-Tmax.m1.R2)~doy,data=TmaxMods,type='l')

plot(Tmax.m1.residsd~doy,data=TmaxMods)
plot(Tmax.m2.residsd~doy,data=TmaxMods)
plot(Tmax.m2.residsd~Tmax.m1.residsd,data=TmaxMods)
abline(0,1)

plot(Tmax.m2.R2~Tmax.m1.R2,data=TmaxMods)
abline(0,1)

pairs(TmaxMods[,1:6])

plot(Tmax.m2.R2~doy,data=TmaxMods)

plot(sommax~doy,data=TmaxMods)

plot(Tmax.elev.slope~doy,data=TmaxMods,col=rainbow(16)[TmaxMods$sommax],pch=19)
legend(10,0.005,legend=1:16,pch=19,col=rainbow(16))
points(Tmax.elev.slope~doy,data=TmaxMods[TmaxMods$Tmax.elev.pval<=0.05,],pch=19)

plot(Tmax.dsol.slope~doy,data=TmaxMods)
points(Tmax.dsol.slope~doy,data=TmaxMods[TmaxMods$Tmax.dsol.pval<=0.05,],pch=19)
abline(h=0)

plot(Tmax.plow500.slope~doy,data=TmaxMods)
points(Tmax.plow500.slope~doy,data=TmaxMods[TmaxMods$Tmax.plow500.pval<=0.05,],pch=19)
abline(h=0)

plot(Tmax.d2fb.slope~doy,data=TmaxMods,col=rainbow(16)[TmaxMods$sommax],pch=19)
points(Tmax.d2fb.slope~doy,data=TmaxMods[TmaxMods$Tmax.d2fb.pval<=0.05,],pch=19)
abline(h=0)

boxplot(Tmax.d2fb.slope~sommax,data=TmaxMods)
abline(h=0,lty=2)

## Do daily weather conditions explain model variation
head(dlySummary)
plot(dlySummary$Tmax,TmaxMods$Tmax.elev.slope)
plot(dlySummary$Tmin,TmaxMods$Tmax.elev.slope)
plot(dlySummary$dtRange,TmaxMods$Tmax.elev.slope)
plot(dlySummary$RHsat.hrs,TmaxMods$Tmax.elev.slope)
plot(dlySummary$Kps.mean_hPa,TmaxMods$Tmax.elev.slope)
plot(dlySummary$Kwspd.mean_m.s,TmaxMods$Tmax.elev.slope)
plot(dlySummary$Tpc2,TmaxMods$Tmax.elev.slope)
plot(Tmax.pcdev$scores[,1],TmaxMods$Tmax.elev.slope)
plot(Tmax.pcdev$scores[,2],TmaxMods$Tmax.elev.slope)
plot(1:366,Tmax.pcdev$scores[,1])

plot(TmaxMods$doy,TmaxMods$Tmax.dsol.slope)
plot(dlySummary$Tmax,TmaxMods$Tmax.dsol.slope)
plot(dlySummary$Tmin,TmaxMods$Tmax.dsol.slope)
plot(dlySummary$dtRange,TmaxMods$Tmax.dsol.slope)
plot(dlySummary$RHsat.hrs,TmaxMods$Tmax.dsol.slope)
plot(dlySummary$Kps.mean_hPa,TmaxMods$Tmax.dsol.slope)
plot(dlySummary$Kwspd.mean_m.s,TmaxMods$Tmax.dsol.slope)
plot(dlySummary$Tpc2,TmaxMods$Tmax.dsol.slope)
plot(Tmax.pcdev$scores[,1],TmaxMods$Tmax.dsol.slope)
plot(Tmax.pcdev$scores[,2],TmaxMods$Tmax.dsol.slope)
plot(1:366,Tmax.pcdev$scores[,1])
plot(1:366,Tmax.pcdev$scores[,2])

plot(TmaxMods$doy,TmaxMods$Tmax.plow500.slope)
plot(dlySummary$Tmax,TmaxMods$Tmax.plow500.slope)
plot(dlySummary$Tmin,TmaxMods$Tmax.plow500.slope)
plot(dlySummary$dtRange,TmaxMods$Tmax.plow500.slope)
plot(dlySummary$RHsat.hrs,TmaxMods$Tmax.plow500.slope)
plot(dlySummary$Kps.mean_hPa,TmaxMods$Tmax.plow500.slope)
plot(dlySummary$Kwspd.mean_m.s,TmaxMods$Tmax.plow500.slope)
plot(dlySummary$Tpc2,TmaxMods$Tmax.plow500.slope)
plot(Tmax.pcdev$scores[,1],TmaxMods$Tmax.plow500.slope)
plot(Tmax.pcdev$scores[,2],TmaxMods$Tmax.plow500.slope)
plot(1:366,Tmax.pcdev$scores[,1])
plot(1:366,Tmax.pcdev$scores[,2])

plot(TmaxMods$doy,TmaxMods$Tmax.d2fb.slope)
plot(dlySummary$Tmax,TmaxMods$Tmax.d2fb.slope)
plot(dlySummary$Tmin,TmaxMods$Tmax.d2fb.slope)
plot(dlySummary$dtRange,TmaxMods$Tmax.d2fb.slope)
plot(dlySummary$RHsat.hrs,TmaxMods$Tmax.d2fb.slope)
plot(dlySummary$Kps.mean_hPa,TmaxMods$Tmax.d2fb.slope)
plot(dlySummary$Kwspd.mean_m.s,TmaxMods$Tmax.d2fb.slope)
plot(dlySummary$Tpc2,TmaxMods$Tmax.d2fb.slope)
plot(Tmax.pcdev$scores[,1],TmaxMods$Tmax.d2fb.slope)
plot(Tmax.pcdev$scores[,2],TmaxMods$Tmax.d2fb.slope)
plot(1:366,Tmax.pcdev$scores[,1])
plot(1:366,Tmax.pcdev$scores[,2])

pairs(cbind(TmaxMods[,1:6],Tmax.pcdev$scores[,1:2]))

pall <- brewer.pal(6,"OrRd")
plot(TmaxMods$Tmax.m2.R2~TmaxMods$Tmax.elev.slope,pch=19,col=pall[cut(dlySummary$Tmax,6)])
abline(v=0,lty=2)

plot(TmaxMods$Tmax.m2.R2~TmaxMods$Tmax.plow500.slope,pch=19,col=pall[cut(dlySummary$Tmax,6)])
abline(v=0,lty=2)

plot(TmaxMods$Tmax.m2.R2~TmaxMods$Tmax.dsol.slope,pch=19,col=pall[cut(dlySummary$Tmax,6)])
abline(v=0,lty=2)

plot(TmaxMods$Tmax.m2.R2~TmaxMods$Tmax.d2fb.slope,pch=19,col=pall[cut(dlySummary$Tmax,6)])
abline(v=0,lty=2)

plot(TmaxMods$Tmax.m2.R2~TmaxMods$Tmax.d2fb.slope,pch=19,col=rainbow(12)[cut(1:366,12)])
#identify(TmaxMods$Tmax.m2.R2~TmaxMods$Tmax.d2fb.slope,n=3)
abline(v=0,lty=2)

plot(TmaxMods$Tmax.elev.slope~TmaxMods$Tmax.d2fb.slope,pch=19,col=pall[cut(TmaxMods$Tmax.m2.R2,6)])
#identify(TmaxMods$Tmax.elev.slope~TmaxMods$Tmax.d2fb.slope,n=3)

plot(TmaxMods$Tmax.elev.slope~TmaxMods$Tmax.d2fb.slope,pch=19,col=pall[cut(dlySummary$Tmax,6)])
plot(TmaxMods$Tmax.elev.slope~TmaxMods$Tmax.d2fb.slope,pch=19,col=pall[cut(dlySummary$VPmax,6)])

plot(Tmax.pcdev$scores[,1]~TmaxMods$Tmax.m2.R2,pch=19,col=cut(dlySummary$Tmax,5))
abline(h=0,lty=2)

## Trying to figure out what's happening on lowest R2 days. False Bay influence brings down R2 in general, but not to lowest values. Lowest R2: days 256, 299, 308
dim(cv)
plot(topo10$elevation,cv[,308])
plot(topo10$d2at,cv[,308])
plot(dlySol[,'rad_tot_308'],cv[,308])

plot(topo10$elevation,cv[,256])
plot(topo10$d2at,cv[,256])
plot(dlySol[,'rad_tot_308'],cv[,256])

plot(topo10$elevation,cv[,299])
plot(topo10$d2at,cv[,299])
plot(dlySol[,'rad_tot_308'],cv[,299])

dim(cv)
head(cv)
class(cv)
summary(cv)
Tmax.max <- apply(cv,1,max,na.rm=F)
plot(topo10$elevation,Tmax.max)


## START TMIN HERE
## Now explore models for Tmin and come up with best

TminMods <- data.frame(doy=1:366,Tmin.m1.R2=NA,Tmin.m1.residsd=NA,Tmin.m1.elev.slope=NA,Tmin.m2.R2=NA,Tmin.m2.residsd=NA,Tmin.elev.slope=NA,Tmin.plow500.slope=NA,Tmin.slope.slope=NA,Tmin.d2fb.slope=NA,Tmin.f5.slope=NA,Tmin.elev.pval=NA,Tmin.plow500.pval=NA,Tmin.slope.pval=NA,Tmin.d2fb.pval=NA,Tmin.f5.pval=NA)
dim(TminMods)

cv <- cbvar$Tmin
class(cv)
head(cv)

# are
plot(plow500~slope,data=topo10)

# Initial tests used to select variables that perform best across the season
# see directory: Tmin-model-selection for nine alternative models, leading to selection of this one - like Tmax with slope instead of dsol - nice symmetry!
d=1
for (d in 1:366) {
    if (d %% 10 == 0) print(d)
    cv[meta$use4Analyses==0,] <- NA
    dsol <- dlySol[,paste('rad_tot_',days[d],sep='')]
    
    fit1 <- lm(cv[,d]~elevation,data=topo10)
    TminMods$Tmin.m1.R2[d] <- summary(fit1)$r.s
    TminMods$Tmin.m1.residsd[d] <- sd(fit1$residuals)
    TminMods$Tmin.m1.elev.slope[d] <- fit1$coefficients[2]
    
    fit2 <- lm(cv[,d]~elevation+plow500+slope+d2fb,data=topo10)
    fit2.1 <- lm(cv[,d]~plow500+slope+d2fb,data=topo10)
    fit2.2 <- lm(cv[,d]~elevation+slope+d2fb,data=topo10)
    fit2.3 <- lm(cv[,d]~elevation+plow500+d2fb,data=topo10)
    fit2.4 <- lm(cv[,d]~elevation+plow500+slope,data=topo10)
    TminMods$Tmin.m2.R2[d] <- summary(fit2)$r.s
    TminMods$Tmin.m2.residsd[d] <- sd(fit2$residuals)
    TminMods$Tmin.elev.slope[d] <- fit2$coefficients[2]
    TminMods$Tmin.plow500.slope[d] <- fit2$coefficients[3]
    TminMods$Tmin.slope.slope[d] <- fit2$coefficients[4]
    TminMods$Tmin.d2fb.slope[d] <- fit2$coefficients[5]
    #TminMods$Tmin.f5.slope[d] <- fit2$coefficients[6]
    TminMods$Tmin.elev.pval[d] <- anova(fit2,fit2.1)$P[2]
    TminMods$Tmin.plow500.pval[d] <- anova(fit2,fit2.2)$P[2]
    TminMods$Tmin.slope.pval[d] <- anova(fit2,fit2.3)$P[2]
    TminMods$Tmin.d2fb.pval[d] <- anova(fit2,fit2.4)$P[2]
    #TminMods$Tmin.f5.pval[d] <- anova(fit2)$P[5]
}
TminMods <- merge(TminMods,syn[,c('doy','sommin')],all=T)
head(TminMods)
tail(TminMods)

plot(Tmin.m1.R2~doy,data=TminMods)
plot(Tmin.m2.R2~doy,data=TminMods)
plot(Tmin.m2.R2~Tmin.m1.R2,data=TminMods)
abline(0,1)

plot(Tmin.m1.residsd~doy,data=TminMods)
plot(Tmin.m2.residsd~doy,data=TminMods)
plot(Tmin.m2.residsd~Tmin.m1.residsd,data=TminMods)
abline(0,1)

pairs(TminMods[,1:6])

mean(TminMods$Tmin.elev.slope) #6.8° per 1000m
length(which(TminMods$Tmin.elev.pval<=0.05)) #361 days
plot(Tmin.elev.slope~doy,data=TminMods)
points(Tmin.elev.slope~doy,data=TminMods[TminMods$Tmin.elev.pval<=0.05,],pch=19)

# plow500
mean(TminMods$Tmin.plow500.slope) # +0.24° per 10% more lower cells
length(which(TminMods$Tmin.plow500.pval<=0.05)) #232 days
plot(Tmin.plow500.slope~doy,data=TminMods) # pretty even through year
points(Tmin.plow500.slope~doy,data=TminMods[TminMods$Tmin.plow500.pval<=0.05,],pch=19)

# slope 
mean(TminMods$Tmin.slope.slope) #2.1° per radian
length(which(TminMods$Tmin.slope.pval<=0.05)) #320 days
plot(Tmin.slope.slope~doy,data=TminMods) # a little stronger in winter
points(Tmin.slope.slope~doy,data=TminMods[TminMods$Tmin.slope.pval<=0.05,],pch=19)

plot(Tmin.plow500.slope~Tmin.slope.slope,data=TminMods) # pretty even through year


# distance to false bay - d2fb
mean(TminMods$Tmin.d2fb.slope) # +3.6e-5° per m
length(which(TminMods$Tmin.d2fb.pval<=0.05)) #157 days
plot(Tmin.d2fb.slope~doy,data=TminMods) # strongest effects in summer (positive slopes)
points(Tmin.d2fb.slope~doy,data=TminMods[TminMods$Tmin.d2fb.pval<=0.05,],pch=19)

# with synoptics
plot(Tmin.d2fb.slope~doy,data=TminMods,col=rainbow(16)[TminMods$sommin],pch=19,xlim=c(1,450))
legend(400,max(TminMods$Tmin.d2fb.slope),pch=19,legend=1:16,col=rainbow(16))

#plot(Tmin.f5.slope~doy,data=TminMods)
#points(Tmin.f5.slope~doy,data=TminMods[TminMods$Tmin.f5.pval<=0.05,],pch=19)

## Do daily weather conditions explain model variation
head(dlySummary)
plot(1:366,TminMods$Tmin.m2.R2)
plot(dlySummary$Tmin,TminMods$Tmin.m2.R2)
plot(dlySummary$dtRange,TminMods$Tmin.m2.R2)
plot(dlySummary$RHsat.hrs,TminMods$Tmin.m2.R2)
plot(dlySummary$Kps.mean_hPa,TminMods$Tmin.m2.R2)
plot(dlySummary$Kwspd.mean_m.s,TminMods$Tmin.m2.R2)
plot(dlySummary$Tpc2,TminMods$Tmin.m2.R2)
#plot(Tmin.pcdev$scores[,1],TminMods$Tmin.m2.R2)
#plot(Tmin.pcdev$scores[,2],TminMods$Tmin.m2.R2)
#plot(1:366,Tmin.pcdev$scores[,1])

# elevation
head(dlySummary)
pcvec <- rep(1,366)
pcvec[TminMods$Tmin.elev.pval<=0.05] <- 19
plot(1:366,TminMods$Tmin.elev.slope,pch=pcvec)
plot(dlySummary$Tmin,TminMods$Tmin.elev.slope,pch=pcvec)
plot(dlySummary$dtRange,TminMods$Tmin.elev.slope,pch=pcvec)
plot(dlySummary$RHsat.hrs,TminMods$Tmin.elev.slope,pch=pcvec)
plot(dlySummary$Kps.mean_hPa,TminMods$Tmin.elev.slope,pch=pcvec)
plot(dlySummary$Kwspd.mean_m.s,TminMods$Tmin.elev.slope,pch=pcvec)
plot(dlySummary$Tpc2,TminMods$Tmin.elev.slope,pch=pcvec)
#plot(Tmin.pcdev$scores[,1],TminMods$Tmin.elev.slope)
#plot(Tmin.pcdev$scores[,2],TminMods$Tmin.elev.slope)
#plot(1:366,Tmin.pcdev$scores[,1])

#f2 = slope
pcvec <- rep(1,366)
pcvec[TminMods$Tmin.slope.pval<=0.05] <- 19
plot(TminMods$doy,TminMods$Tmin.slope.slope,pch=pcvec)
plot(dlySummary$Tmin,TminMods$Tmin.slope.slope,pch=pcvec)
plot(dlySummary$dtRange,TminMods$Tmin.slope.slope,pch=pcvec)
plot(dlySummary$RHmax,TminMods$Tmin.slope.slope,pch=pcvec)
plot(dlySummary$RHsat.hrs,TminMods$Tmin.slope.slope,pch=pcvec)
plot(dlySummary$Kps.mean_hPa,TminMods$Tmin.slope.slope,pch=pcvec)
plot(dlySummary$Kwspd.mean_m.s,TminMods$Tmin.slope.slope,pch=pcvec)
plot(dlySummary$Tpc2,TminMods$Tmin.slope.slope,pch=pcvec)
#plot(Tmin.pcdev$scores[,1],TminMods$Tmin.dsol.slope)
#plot(Tmin.pcdev$scores[,2],TminMods$Tmin.dsol.slope)
#plot(1:366,Tmin.pcdev$scores[,1])
#plot(1:366,Tmin.pcdev$scores[,2])

# plow500
pcvec <- rep(1,366)
pcvec[TminMods$Tmin.plow500.pval<=0.05] <- 19
plot(TminMods$doy,TminMods$Tmin.plow500.slope,pch=pcvec)
plot(dlySummary$Tmin,TminMods$Tmin.plow500.slope,pch=pcvec)
plot(dlySummary$dtRange,TminMods$Tmin.plow500.slope,pch=pcvec)
plot(dlySummary$RHsat.hrs,TminMods$Tmin.plow500.slope,pch=pcvec)
plot(dlySummary$Kps.mean_hPa,TminMods$Tmin.plow500.slope,pch=pcvec)
plot(dlySummary$Kwspd.mean_m.s,TminMods$Tmin.plow500.slope,pch=pcvec)
plot(dlySummary$Tpc2,TminMods$Tmin.plow500.slope,pch=pcvec)
#plot(Tmin.pcdev$scores[,1],TminMods$Tmin.plow500.slope)
#plot(Tmin.pcdev$scores[,2],TminMods$Tmin.plow500.slope)
#plot(1:366,Tmin.pcdev$scores[,1])
#plot(1:366,Tmin.pcdev$scores[,2])

plot(TminMods$doy,TminMods$Tmin.d2fb.slope,pch=pcvec)
plot(dlySummary$Tmin,TminMods$Tmin.d2fb.slope,pch=pcvec)
plot(dlySummary$Tmin,TminMods$Tmin.d2fb.slope,pch=pcvec)
plot(dlySummary$dtRange,TminMods$Tmin.d2fb.slope,pch=pcvec)
plot(dlySummary$RHsat.hrs,TminMods$Tmin.d2fb.slope,pch=pcvec)
plot(dlySummary$Kps.mean_hPa,TminMods$Tmin.d2fb.slope,pch=pcvec)
plot(dlySummary$Kwspd.mean_m.s,TminMods$Tmin.d2fb.slope,pch=pcvec)
plot(dlySummary$Tpc2,TminMods$Tmin.d2fb.slope,pch=pcvec)
#plot(Tmin.pcdev$scores[,1],TminMods$Tmin.d2fb.slope,pch=pcvec)
#plot(Tmin.pcdev$scores[,2],TminMods$Tmin.d2fb.slope,pch=pcvec)
#plot(1:366,Tmin.pcdev$scores[,1],pch=pcvec)
#plot(1:366,Tmin.pcdev$scores[,2],pch=pcvec)

pairs(cbind(TminMods[,1:6],Tmin.pcdev$scores[,1:2]))

pall <- brewer.pal(6,"OrRd")
plot(TminMods$Tmin.m2.R2~TminMods$Tmin.elev.slope,pch=19,col=pall[cut(dlySummary$Tmin,6)])
abline(v=0,lty=2)

plot(TminMods$Tmin.m2.R2~TminMods$Tmin.elev.slope,pch=19,col=pall[cut(dlySummary$Tmin,6)])
abline(v=0,lty=2)

plot(TminMods$Tmin.m2.R2~TminMods$Tmin.plow500.slope,pch=19,col=pall[cut(dlySummary$Tmin,6)])
abline(v=0,lty=2)

plot(TminMods$Tmin.m2.R2~TminMods$Tmin.slope.slope,pch=19,col=pall[cut(dlySummary$Tmin,6)])
abline(v=0,lty=2)

plot(TminMods$Tmin.m2.R2~TminMods$Tmin.d2fb.slope,pch=19,col=rainbow(12)[cut(1:366,12)])
#identify(TminMods$Tmin.m2.R2~TminMods$Tmin.d2fb.slope,n=3)
abline(v=0,lty=2)

plot(TminMods$Tmin.elev.slope~TminMods$Tmin.d2fb.slope,pch=19,col=pall[cut(TminMods$Tmin.m2.R2,6)])
#identify(TminMods$Tmin.elev.slope~TminMods$Tmin.d2fb.slope,n=3)

plot(TminMods$Tmin.elev.slope~TminMods$Tmin.d2fb.slope,pch=19,col=pall[cut(dlySummary$Tmin,6)])
plot(TminMods$Tmin.elev.slope~TminMods$Tmin.d2fb.slope,pch=19,col=pall[cut(dlySummary$VPmax,6)])

plot(Tmin.pcdev$scores[,1]~TminMods$Tmin.m2.R2,pch=19,col=cut(dlySummary$Tmin,5))
abline(h=0,lty=2)
    
## COMPARE TMIN AND TMAX MODELS
plot(TminMods$Tmin.elev.slope~TmaxMods$Tmax.elev.slope)
abline(0,1)
abline(h=0);abline(v=0)
abline(h=-0.0068,lty=2);abline(v=-0.0068,lty=2)

plot(TminMods$Tmin.plow500.slope~TmaxMods$Tmax.plow500.slope)

plot(TminMods$Tmin.d2fb.slope~TmaxMods$Tmax.d2fb.slope)
abline(h=0);abline(v=0)

### redoing code above here to use cbvar instead of cbday
i=1
for (i in 1:366) {
    if (i %% 20 == 0) print(i)
    cd <- cbday[[i]]
    #head(cd)
    cd <- merge(cd,topo10)
    #dim(cd)
    #head(cd)
    dayMods$Tmax.mn[i] <- mean(cd$Tmax,na.rm=T)
    dayMods$Tmin.mn[i] <- mean(cd$Tmin,na.rm=T)
    dayMods$dtRange[i] <- dayMods$Tmax.mn[i]-dayMods$Tmin.mn[i]
    dayMods$VPmax.mn[i]<- mean(cd$VPmax,na.rm=T)
    dayMods$RHsatHrs.mn[i]<- mean(cd$RHsat.hrs,na.rm=T)
    
    #plot(Tmax~elevation,data=cd)
    #plot(Tmin~elevation,data=cd)
    #plot(VPmax~elevation,data=cd)
    #plot(RHsat.hrs~elevation,data=cd)
    
    fit.el <- lm(Tmax~elevation,data=cd)
    dayMods$Tmax.m1.R2[i] <- summary(fit.el)$r.s
    fit2 <- lm(Tmax~elevation+thl,data=cd)
    dayMods$Tmax.m2.R2[i] <- summary(fit2)$r.s
    fit4 <- lm(Tmax~elevation+thl+d2at+d2fb,data=cd)
    dayMods$Tmax.m4.R2[i] <- summary(fit4)$r.s
    dayMods$Tmax.elev.slope[i] <- summary(fit4)$coeff[2,1]
    dayMods$Tmax.thl.slope[i] <- summary(fit4)$coeff[3,1]
    dayMods$Tmax.d2at.slope[i] <- summary(fit4)$coeff[4,1]
    dayMods$Tmax.d2fb.slope[i] <- summary(fit4)$coeff[5,1]
    
    fit.el <- lm(Tmin~elevation,data=cd)
    dayMods$Tmin.m1.R2[i] <- summary(fit.el)$r.s
    fit2 <- lm(Tmin~elevation+plow500,data=cd)
    dayMods$Tmin.m2.R2[i] <- summary(fit2)$r.s
    #fit3t <- lm(Tmin~elevation+tpi500+slope,data=cd)
    fit3p <- lm(Tmin~elevation+plow500+slope,data=cd)
    dayMods$Tmin.m3.R2[i] <- summary(fit3p)$r.s
    dayMods$Tmin.elev.slope[i] <- summary(fit3p)$coeff[2,1]
    dayMods$Tmin.plow.slope[i] <- summary(fit3p)$coeff[3,1]
    dayMods$Tmin.slp.slope[i] <- summary(fit3p)$coeff[4,1]
    #hist(fit3$residuals)
}

head(dayMods)
plot(dayMods$Tmax.mn,type='l')
plot(dayMods$Tmin.mn,type='l')
plot(dayMods$dtRange,type='l')
plot(dayMods$VPmax.mn,type='l')
plot(dayMods$RHsatHrs.mn,type='l')
pairs(dayMods[,2:6])

plot(dayMods$Tmax.m1.R2,type='l')
plot(dayMods$Tmax.m2.R2,type='l')
plot(dayMods$Tmax.m4.R2,type='l')
plot(dayMods$Tmax.m4.R2-dayMods$Tmax.m1.R2,type='l')
plot(Tmax.m4.R2~RHsatHrs.mn,data=dayMods)
plot(Tmax.m4.R2~dtRange,data=dayMods)

plot(dayMods$Tmax.elev.slope,type='l')
plot(Tmax.elev.slope~dtRange,data=dayMods)
plot(dayMods$Tmax.thl.slope,type='l')
plot(Tmax.thl.slope~dtRange,data=dayMods)
plot(Tmax.thl.slope~Tmax.mn,data=dayMods)
plot(dayMods$Tmax.d2fb.slope,type='l')
plot(Tmax.d2fb.slope~Tmax.mn,data=dayMods)
plot(dayMods$Tmax.d2at.slope,type='l')
plot(Tmax.d2at.slope~Tmax.mn,data=dayMods)

plot(dayMods$Tmin.m1.R2,type='l')
plot(dayMods$Tmin.m2.R2,type='l')
plot(dayMods$Tmin.m3.R2,type='l')
plot(Tmin.m1.R2~RHsatHrs.mn,data=dayMods)
plot(Tmin.m1.R2~dtRange,data=dayMods)

plot(dayMods$Tmin.elev.slope,type='l')
plot(Tmin.elev.slope~dtRange,data=dayMods)
plot(Tmin.elev.slope~RHsatHrs.mn,data=dayMods)

#What does gradient look like when it reverses
which.max(dayMods$Tmin.elev.slope)
plot(topo10$elevation,cbday[[308]]$Tmin)

plot(dayMods$Tmin.plow.slope,type='l')
plot(dayMods$Tmin.slp.slope,type='l')
plot(Tmin.plow.slope~RHsatHrs.mn,data=dayMods)
plot(Tmin.slp.slope~RHsatHrs.mn,data=dayMods)

## What kinds of days are there?
head(dayMods)
names(dayMods)[c(2:6,9:13,16:19)]
pc.days <- princomp(dayMods[,c(2:6,9:13,16:19)],cor=T)
biplot(pc.days)
pc.days$loadings

plot(Tmax.m4.R2~Tmin.m3.R2,data=dayMods)
summary(lm(Tmax.m4.R2~Tmin.m3.R2,data=dayMods))
plot(Tmax.m4.R2~doy,data=dayMods,type='b')
points(Tmin.m3.R2~doy,data=dayMods,col='blue',type='b')
plot(Tmax.m4.R2~dtRange,data=dayMods)
plot(Tmin.m3.R2~RHsatHrs.mn,data=dayMods)

### END DAY MODELS

### Daily extreme models
xday <- read.csv('/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/Rdata.files/xday.csv')
head(xday)
summary(xday[,-1])
# RH saturating isn't interesting, all sites 0-24

plot(xday$Tmax.max~topo10$elevation,ylim=c(min(xday$Tmax.min,na.rm=T),max(xday$Tmax.max,na.rm=T)))
points(xday$Tmax.min~topo10$elevation)

plot(xday$Tmax.min~topo10$elevation,ylim=c(min(xday$Tmin.min,na.rm=T),max(xday$Tmin.max,na.rm=T)))
points(xday$Tmin.min~topo10$elevation,col='blue')
abline(0,0,lty=2)

plot(xday$VPmax.max~topo10$elevation,ylim=c(min(xday$VPmax.min,na.rm=T),max(xday$VPmax.max,na.rm=T)))
points(xday$VPmax.min~topo10$elevation)

xday <- merge(xday,topo10)

fit.el <- lm(Tmax.max~elevation,data=xday)
summary(fit.el)
fit2 <- lm(Tmax.max~elevation+thl,data=xday)
summary(fit2)
fit4 <- lm(Tmax.max~elevation+thl+d2at+d2fb,data=xday)
summary(fit4)

fit.el <- lm(Tmax.min~elevation,data=xday)
summary(fit.el)
fit2 <- lm(Tmax.min~elevation+thl,data=xday)
summary(fit2)
fit4 <- lm(Tmax.min~elevation+thl+d2at+d2fb,data=xday)
summary(fit4)

fit.el <- lm(Tmin.max~elevation,data=xday)
summary(fit.el)
fit2 <- lm(Tmin.max~elevation+plow500,data=xday)
summary(fit2)
#fit3t <- lm(Tmin~elevation+tpi500+slope,data=cd)
fit3p <- lm(Tmin.max~elevation+plow500+slope,data=xday)
summary(fit3p)

fit.el <- lm(Tmin.min~elevation,data=xday)
summary(fit.el)
fit2 <- lm(Tmin.min~elevation+plow500,data=xday)
summary(fit2)
#fit3t <- lm(Tmin~elevation+tpi500+slope,data=cd)
fit3p <- lm(Tmin.min~elevation+plow500+slope,data=xday)
summary(fit3p)

## Examine matrices by variable
load('/Users/david/Documents/Projects/TableMtProject/Microclimates/Field_data_2012/data/Rdata.files/cbvar.Rdata')
tmin <- cbvar[[1]]
tmin.site.mn <- apply(tmin,2,mean,na.rm=T)

yl <- range(tmin,na.rm=T)
plot(1:366,tmin[1,],ylim=yl,type='l')
for (i in 2:95) points(1:366,tmin[i,],type='l')
plot(1:366,tmin[1,],ylim=yl,type='l')

# plot profiles across sites, sorted by site
cc <- complete.cases(tmin)
tmin.cc <- tmin[cc,]
tmin.site.mns <- apply(tmin.cc,1,mean)
tmin.cc <- tmin.cc[order(tmin.site.mns),]
tmin.day.mns <- apply(tmin.cc,2,mean)
tmin.cc <- tmin.cc[,order(tmin.day.mns)]
head(tmin.cc)

plot(tmin.cc[,1],type='l',ylim=yl)
for (i in 1:366) points(tmin.cc[,i],type='l')

plot(1:366,tmin.cc[1,],type='l',ylim=yl)
for (i in 1:83) points(1:366,tmin.cc[i,],type='l')

## cluster days for one variable at a time
require(vegan)
head(tmin.cc)
dim(tmin.cc)
dc <- kmeans(tmin.cc,4)
head(dc)
plot(tmin.cc[,c(100,366)],col=dc$cluster)


############ END - from here on is old
#### bring script up from here adjusting to daily models
fname <- paste(hdir,'Microclimates/Field_data_2012/data/Rdata.files/cm12.Rdata',sep='')
load(fname)
read.table(paste(hdir,'Microclimates/Field_data_2012/data/Rdata.files/cm12_metadata.txt',sep=''))
dim(cm12)
head(cm12)
dim(cm12)

# extract site level data
sites <- sort(levels(cm12$mt.siteID))
cm.sites <- cm12[match(sites,cm12$mt.siteID),]
head(cm.sites)
dim(cm.sites)
cm.sites <- cm.sites[,-c(5:14)]
head(cm.sites)

# Obtain annual climate stats                                                                                                                                                                                                                                         names(cm12)
cm.sites$Tmin.minyr <- tapply(cm12$Tmin.min,cm12$mt.siteID,min,na.rm=T)
cm.sites$Tmin.mean.minyr <- tapply(cm12$Tmin.mean,cm12$mt.siteID,min,na.rm=T)
cm.sites$Tmin.meanyr <- tapply(cm12$Tmin.mean,cm12$mt.siteID,mean,na.rm=T)
cm.sites$Tmean.meanyr <- tapply(cm12$Tmean.mean,cm12$mt.siteID,mean,na.rm=T)
cm.sites$Tmax.meanyr <- tapply(cm12$Tmax.mean,cm12$mt.siteID,mean,na.rm=T)
cm.sites$Tmax.mean.maxyr <- tapply(cm12$Tmax.mean,cm12$mt.siteID,max,na.rm=T)
cm.sites$Tmax.maxyr <- tapply(cm12$Tmax.max,cm12$mt.siteID,max,na.rm=T)
cm.sites$RHsat.hrs.minyr <- tapply(cm12$RHsat.hrs,cm12$mt.siteID,min,na.rm=T)
cm.sites$RHsat.hrs.meanyr <- tapply(cm12$RHsat.hrs,cm12$mt.siteID,mean,na.rm=T)
cm.sites$RHsat.hrs.maxyr <- tapply(cm12$RHsat.hrs,cm12$mt.siteID,max,na.rm=T)

head(cm.sites)
summary(cm.sites)

## Overall model using Upper Station of Cable Car as 'well-mixed' standard to take care of seasonal variation
CWU.tmax <- cm12[cm12$mt.siteID=='TB.CWU','Tmax.mean']
CWU.tmin <- cm12[cm12$mt.siteID=='TB.CWU','Tmin.mean']
cm12$CWU.tmax <- CWU.tmax[cm12$mn]
cm12$CWU.tmin <- CWU.tmin[cm12$mn]
head(cm12)

# Tmin analysis
pall <- rainbow(12)
plot(cm12$elev,cm12$Tmin.mean,type='n')
for (i in 1:12) {
    sel <- cm12$mn==i
    points(cm12$Tmin.mean[sel]~cm12$elev[sel],col=pall[i])
    abline(lm(cm12$Tmin.mean[sel]~cm12$elev[sel]),col=pall[i])
}
points(cm12$elev[cm12$mt.siteID=='TB.CWU'],CWU.tmin,col=pall,pch=19)
fit <- lm(Tmin.mean~elev+CWU.tmin,data=cm12)
summary(fit)
hist(fit$residuals)
sd(fit$residuals)

fitf <- lm(Tmin.mean~elev+plow500.30m+slp30m+solar_rad+dist2falsebay+CWU.tmin,data=cm12)
summary(fitf)
hist(fitf$residuals)
sd(fitf$residuals)

op=par(mfrow=c(2,1))
hist(fit$residuals)
hist(fitf$residuals)
par(op)

plot(fit$residuals,fitf$residuals)

# Tmax analysis
pall <- rainbow(12)
plot(cm12$elev,cm12$Tmax.mean,type='n')
for (i in 1:12) {
    sel <- cm12$mn==i
    points(cm12$Tmax.mean[sel]~cm12$elev[sel],col=pall[i])
    abline(lm(cm12$Tmax.mean[sel]~cm12$elev[sel]),col=pall[i])
}
points(cm12$elev[cm12$mt.siteID=='TB.CWU'],CWU.tmax,col=pall,pch=19)
fit <- lm(Tmax.mean~elev+CWU.tmax,data=cm12)
summary(fit)
hist(fit$residuals)
sd(fit$residuals)

fitf <- lm(Tmax.mean~elev+plow500.30m+slp30m+solar_rad+dist2falsebay+CWU.tmax,data=cm12)
summary(fitf)
hist(fitf$residuals)
sd(fitf$residuals)

op=par(mfrow=c(2,1))
hist(fit$residuals)
hist(fitf$residuals)
par(op)

plot(fit$residuals,fitf$residuals)


## LOOP THROUGH MONTHLY MODELS

## TMAX.MEAN MODELS
fmax1 <- list()
fmax2 <- list()
for (i in 1:12) {
    cm <- cm12[cm12$mn==i,]
    dim(cm)
    head(cm)
    cor(cm[,c('dem30m','tpi500.30m','plow500.30m','slp30m','solar_rad','dist2falsebay','dist2atlantic')],use='pair')
    fmax1[[i]] <- lm(Tmax.mean~dem30m+plow500.30m+solar_rad+dist2falsebay,data=cm)
    print(summary(fmax1[[i]])$r.sq)
    fmax2[[i]] <- lm(Tmax.mean~dem30m+plow500.30m+solar_rad+dist2atlantic,data=cm)
    print(summary(fmax2[[i]])$r.sq)
}

#plot tmax r-sq by month
fmax1r <- summary(fmax1[[1]])$r.sq
for (i in 2:12) fmax1r <- c(fmax1r,summary(fmax1[[i]])$r.sq)
fmax2r <- summary(fmax2[[1]])$r.sq
for (i in 2:12) fmax2r <- c(fmax2r,summary(fmax2[[i]])$r.sq)
plot(1:12,fmax1r,type='l',ylim=range(c(fmax1r,fmax2r)))
points(1:12,fmax2r,type='l',lty=2)

#plot Tmax tvalues by month
fmax1t <- summary(fmax1[[1]])$coeff[,3]
for (i in 2:12) fmax1t <- rbind(fmax1t,summary(fmax1[[i]])$coeff[,3])
plot(1:12,fmax1t[,2],type='l',ylim=c(min(fmax1t[,-1]),max(fmax1t[,-1])))
points(1:12,fmax1t[,3],type='l',lwd=2)
points(1:12,fmax1t[,4],type='l',lty=2)
points(1:12,fmax1t[,5],type='l',lty=3)
legend(2,-5,legend=c('dem','plow','solar','slp','dist2fb'),lty=c(1,1,2,3,4),lwd=c(1,2,1,1,1))
abline(h=c(-1.96,1.96))

fmax2t <- summary(fmax2[[1]])$coeff[,3]
for (i in 2:12) fmax2t <- rbind(fmax2t,summary(fmax2[[i]])$coeff[,3])
plot(1:12,fmax2t[,2],type='l',ylim=c(min(fmax2t[,-1]),max(fmax2t[,-1])))
points(1:12,fmax2t[,3],type='l',lwd=2)
points(1:12,fmax2t[,4],type='l',lty=2)
points(1:12,fmax2t[,5],type='l',lty=3)

#plot slopes by month
fmax1s <- summary(fmax1[[1]])$coeff[,1]
for (i in 2:12) fmax1s <- rbind(fmax1s,summary(fmax1[[i]])$coeff[,1])
plot(1:12,fmax1s[,2],type='l')
plot(1:12,fmax1s[,3],type='l',lwd=2)
plot(1:12,fmax1s[,4],type='l',lty=2)
plot(1:12,fmax1s[,5],type='l',lty=3)

fmax2t <- summary(fmax2[[1]])$coeff[,3]
for (i in 2:12) fmax2t <- rbind(fmax2t,summary(fmax2[[i]])$coeff[,3])
plot(1:12,fmax2t[,2],type='l',ylim=c(min(fmax2t[,-1]),max(fmax2t[,-1])))
points(1:12,fmax2t[,3],type='l',lwd=2)
points(1:12,fmax2t[,4],type='l',lty=2)
points(1:12,fmax2t[,5],type='l',lty=3)

## TMIN.MEAN MODELS
fmin1 <- list()
fmin2 <- list()
for (i in 1:12) {
    cm <- cm12[cm12$mn==i,]
    dim(cm)
    head(cm)
    cor(cm[,c('dem30m','tpi500.30m','plow500.30m','slp30m','solar_rad','dist2falsebay','dist2atlantic')],use='pair')
    fmin1[[i]] <- lm(Tmin.mean~dem30m+plow500.30m+solar_rad+slp30m+dist2falsebay,data=cm)
    #print(summary(fmin1[[i]])$r.sq)
    fmin2[[i]] <- lm(Tmin.mean~dem30m+plow500.30m+solar_rad+slp30m+dist2atlantic,data=cm)
    #print(summary(fmin2[[i]])$r.sq)
}

#plot tmin r-sq by month
fmin1r <- summary(fmin1[[1]])$r.sq
for (i in 2:12) fmin1r <- c(fmin1r,summary(fmin1[[i]])$r.sq)
fmin2r <- summary(fmin2[[1]])$r.sq
for (i in 2:12) fmin2r <- c(fmin2r,summary(fmin2[[i]])$r.sq)
plot(1:12,fmin1r,type='l')
points(1:12,fmin2r,type='l',lty=2)

#plot Tmin tvalues by month
fmin1t <- summary(fmin1[[1]])$coeff[,3]
for (i in 2:12) fmin1t <- rbind(fmin1t,summary(fmin1[[i]])$coeff[,3])
plot(1:12,fmin1t[,2],type='l',ylim=c(min(fmin1t[,-1]),max(fmin1t[,-1])))
points(1:12,fmin1t[,3],type='l',lwd=2)
points(1:12,fmin1t[,4],type='l',lty=2)
points(1:12,fmin1t[,5],type='l',lty=3)
points(1:12,fmin1t[,6],type='l',lty=4)
legend(2,-5,legend=c('dem','plow','solar','slp','dist2fb'),lty=c(1,1,2,3,4),lwd=c(1,2,1,1,1))
abline(h=c(-1.96,1.96))

#something funny about this one
fmin2t <- summary(fmin2[[1]])$coeff[,3]
for (i in 2:12) fmin2t <- rbind(fmin2t,summary(fmin2[[i]])$coeff[,3])
plot(1:12,fmin2t[,2],type='l',ylim=range(fmin2t[,-1]))
points(1:12,fmin2t[,3],type='l',lwd=2)
points(1:12,fmin2t[,4],type='l',lty=2)
points(1:12,fmin2t[,5],type='l',lty=3)
points(1:12,fmin2t[,6],type='l',lty=4)

#plot slopes by month
fmin1s <- summary(fmin1[[1]])$coeff[,1]
for (i in 2:12) fmin1s <- rbind(fmin1s,summary(fmin1[[i]])$coeff[,1])
summary(fmin1[[1]])
plot(1:12,fmin1s[,2],type='l')
plot(1:12,fmin1s[,3],type='l',lwd=2)
plot(1:12,fmin1s[,4],type='l',lty=2)
plot(1:12,fmin1s[,5],type='l',lty=3)
plot(1:12,fmin1s[,6],type='l',lty=3)

fmin2t <- summary(fmin2[[1]])$coeff[,1]
for (i in 2:12) fmin2t <- rbind(fmin2t,summary(fmin2[[i]])$coeff[,1])
plot(1:12,fmin2t[,2],type='l')
plot(1:12,fmin2t[,3],type='l',lwd=2)
plot(1:12,fmin2t[,4],type='l',lty=2)
plot(1:12,fmin2t[,5],type='l',lty=3)
plot(1:12,fmin2t[,6],type='l',lty=3)

# slope vs. tval
plot(fmin1t[,2],fmin1s[,2])
plot(fmin1t[,3],fmin1s[,3])
plot(fmin1t[,4],fmin1s[,4])
plot(fmin1t[,5],fmin1s[,5])
plot(fmin1t[,6],fmin1s[,6])

## RHsat.hrs MODELS
frh1 <- list()
frh2 <- list()
for (i in 1:12) {
    cm <- cm12[cm12$mn==i,]
    dim(cm)
    head(cm)
    cor(cm[,c('dem30m','tpi500.30m','plow500.30m','slp30m','solar_rad','dist2falsebay','dist2atlantic')],use='pair')
    frh1[[i]] <- lm(RHsat.hrs~dem30m+plow500.30m+solar_rad+slp30m+dist2falsebay,data=cm)
    #print(summary(fmin1[[i]])$r.sq)
    frh2[[i]] <- lm(RHsat.hrs~dem30m+plow500.30m+solar_rad+slp30m+dist2atlantic,data=cm)
    #print(summary(fmin2[[i]])$r.sq)
}

#plot RH r-sq by month
frh1r <- summary(frh1[[1]])$r.sq
for (i in 2:12) frh1r <- c(frh1r,summary(frh1[[i]])$r.sq)
frh2r <- summary(frh2[[1]])$r.sq
for (i in 2:12) frh2r <- c(frh2r,summary(frh2[[i]])$r.sq)
plot(1:12,frh1r,type='l')
points(1:12,frh2r,type='l',lty=2)

#plot RH tvalues by month
frh1t <- summary(frh1[[1]])$coeff[,3]
for (i in 2:12) frh1t <- rbind(frh1t,summary(frh1[[i]])$coeff[,3])
plot(1:12,frh1t[,2],type='l',ylim=range(frh1t[,-1]))
points(1:12,frh1t[,3],type='l',lwd=2)
points(1:12,frh1t[,4],type='l',lty=2)
points(1:12,frh1t[,5],type='l',lty=3)
points(1:12,frh1t[,6],type='l',lty=4)
legend(2,7,legend=c('dem','plow','solar','slp','dist2fb'),lty=c(1,1,2,3,4),lwd=c(1,2,1,1,1))
abline(h=c(-1.96,1.96))

frh2t <- summary(frh2[[1]])$coeff[,3]
for (i in 2:12) frh2t <- rbind(frh2t,summary(frh2[[i]])$coeff[,3])
plot(1:12,frh2t[,2],type='l',ylim=range(frh2t[,-1]))
points(1:12,frh2t[,3],type='l',lwd=2)
points(1:12,frh2t[,4],type='l',lty=2)
points(1:12,frh2t[,5],type='l',lty=3)
points(1:12,frh2t[,6],type='l',lty=4)

#plot slopes by month
frh1s <- summary(frh1[[1]])$coeff[,1]
for (i in 2:12) frh1s <- rbind(frh1s,summary(frh1[[i]])$coeff[,1])
plot(1:12,frh1s[,2],type='l',ylim=range(frh1s[,-1]))
points(1:12,frh1s[,3],type='l',lwd=2)
points(1:12,frh1s[,4],type='l',lty=2)
points(1:12,frh1s[,5],type='l',lty=3)
points(1:12,frh1s[,6],type='l',lty=4)

frh2s <- summary(frh2[[1]])$coeff[,1]
for (i in 2:12) frh2s <- rbind(frh2s,summary(frh2[[i]])$coeff[,1])
plot(1:12,frh2s[,2],type='l',ylim=range(frh2s[,-1]))
points(1:12,frh2s[,3],type='l',lwd=2)
points(1:12,frh2s[,4],type='l',lty=2)
points(1:12,frh2s[,5],type='l',lty=3)
points(1:12,frh2s[,6],type='l',lty=4)


## Make predicted layers
domain <- raster('/Users/david/Documents/Projects/Dropbox/TableMtProject/Microclimates/rasters/utm_10m/tmsm.domains.grd')
domain <- aggregate(domain,3)

load('/Users/david/Documents/Projects/Dropbox/TableMtProject/Microclimates/Field_data_2012/data/Rdata.files/mon.stack.Rdata')
mon.stack
class(mon.stack[[1]])
plot(mon.stack[[1]])
plot(mon.stack[[1]][[1]])
points(cm.sites[,c('UTM.east','UTM.north')])
points(cm.sites[which.max(cm.sites$elev),c('UTM.east','UTM.north')],pch=19)
cm.sites[which.max(cm.sites$elev),]

plot(predict(mon.stack[[1]],frh1[[1]]))
projection(mon.stack)
plot(mon.stack[[1]])
plot(domain)
domain <- aggregate(domain,3,max)
domain
plot(mon.stack[[1]]*domain)

## these models use dist2falsebay OR dist2atlantic, whichever model has higher r2; means terms switch during season
pred.tmax <- stack()
fmax1r-fmax2r
for (i in 1:12) {
    #if (fmax1r[i] > fmax2r[i]) pred.tmax <- addLayer(pred.tmax,predict(mon.stack[[i]],fmax1[[i]])) else pred.tmax <- addLayer(pred.tmax,predict(mon.stack[[i]],fmax2[[i]]))
    pred.tmax <- addLayer(pred.tmax,predict(mon.stack[[i]],fmax1[[i]]))
}
plot(pred.tmax*domain,main=paste(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),'tmax'))

pred.tmin <- stack()
fmin1r-fmin2r
for (i in 1:12) {
    #if (fmin1r[i] > fmin2r[i]) pred.tmin <- addLayer(pred.tmin,predict(mon.stack[[i]],fmin1[[i]])) else pred.tmin <- addLayer(pred.tmin,predict(mon.stack[[i]],fmin2[[i]]))
    pred.tmin <- addLayer(pred.tmin,predict(mon.stack[[i]],fmin1[[i]]))
}
plot(pred.tmin*domain,main=paste(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),'tmin'))

pred.rh <- stack()
frh1r-frh2r
for (i in 1:12) {
    if (frh1r[i] > frh2r[i]) pred.rh <- addLayer(pred.rh,predict(mon.stack[[i]],frh1[[i]])) else pred.rh <- addLayer(pred.rh,predict(mon.stack[[i]],frh2[[i]]))
}
plot(pred.rh*domain,main=paste(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),'RHsat'))

# make pdf
setwd('/Users/david/Documents/Projects/Dropbox/TableMtProject/Microclimates/Field_data_2012/analysis/figures_May14')
pdf('topoclimates-tmax.pdf',width=8,height=8)
plot(pred.tmax*domain,main=paste(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),'Tmax'))
dev.off()

pdf('topoclimates-tmin.pdf',width=8,height=8)
plot(pred.tmin*domain,main=paste(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),'Tmin'))
dev.off()

pdf('topoclimates-rh.pdf',width=8,height=8)
plot(pred.rh*domain,main=paste(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),'RHsat'))
dev.off()
### UP TO HERE ####





### OLD ESA 2012 MODELS
# Summer - FULL MODEL
names(cmonth)
mdat <- cmonth[cmonth$ym=='2012 01',1:19] 
head(mdat)
topovars <- extract(janstack,cbind(mdat$UTM.east,mdat$UTM.north),
                    method='bilinear')
head(topovars)
mdat <- cbind(mdat,topovars)
head(mdat)

# plot maximum jan temp vs. sensor elevation
plot(mdat$elev,mdat$Tmax.mean)
fit <- lm(Tmax.mean ~ dem30m + month_rad_tot + tpi500.30m
          + plow500.30m + slp30m
           + dist2falsebay,
          data=mdat)

# plot fitted data vs. elevation and on map
fit2mdat <- match(names(fit$fitted.values),rownames(mdat))
plot(mdat$elev[fit2mdat],fit$fitted.values)

length(fit$residuals)
plot(dem30m)
points(mdat$UTM.east[fit2mdat],mdat$UTM.north[fit2mdat],pch=19,
       col=rev(heat.colors(93))[rank(fit$fitted.values)])

# plot residuals vs. elevation and on map
plot(mdat$elev[fit2mdat],fit$residuals)

plot(dem30m)
# red points are more positive residuals, white points more negative
points(mdat$UTM.east[fit2mdat],mdat$UTM.north[fit2mdat],pch=19,
       col=rev(heat.colors(93))[rank(fit$residuals)])

summary(fit)
jan.tmax.pred <- predict(janstack,fit)
plot(jan.tmax.pred*domains)

# fit minimum temp values
plot(mdat$elev,mdat$Tmin.mean)
fit <- lm(Tmin.mean ~ dem30m + month_rad_tot + tpi500.30m
          + plow500.30m + slp30m
          + dist2falsebay,data=mdat)
summary(fit)
jan.tmin.pred <- predict(janstack,fit)
plot(jan.tmin.pred*domains)

# plot fitted data vs. elevation and on map
fit2mdat <- match(names(fit$fitted.values),rownames(mdat))
plot(mdat$elev[fit2mdat],fit$fitted.values)

length(fit$residuals)
plot(dem30m)
points(mdat$UTM.east[fit2mdat],mdat$UTM.north[fit2mdat],pch=19,
       col=rev(heat.colors(93))[rank(fit$fitted.values)])

# plot residuals vs. elevation and on map
plot(mdat$elev[fit2mdat],fit$residuals)
plot(dem30m)
# red points are more positive residuals, white points more negative
points(mdat$UTM.east[fit2mdat],mdat$UTM.north[fit2mdat],pch=19,
       col=rev(heat.colors(93))[rank(fit$residuals)])

summary(fit)
jan.tmax.pred <- predict(janstack,fit)
plot(jan.tmax.pred*domains)

# plot january tmin vs. tmax values across sites
plot(getValues(jan.tmin.pred*domains),
     getValues(jan.tmax.pred*domains),
     pch='.')

###### MATT AND JASPER: EVERYTHING RECHECKED UP TO HERE ######
# SUMMER ELEV ONLY MODEL
fite <- lm(Tmax.mean ~ dem30m 
          + dist2falsebay,
          data=mdat)
summary(fite)
jan.tmax.pred.e <- predict(janstack,fite)
plot(jan.tmax.pred.e*domains)


fite <- lm(Tmin.mean ~ dem30m 
          + dist2falsebay,data=mdat)
summary(fite)
jan.tmin.pred.e <- predict(janstack,fite)
plot(jan.tmin.pred.e*domains)

plot(getValues(jan.tmin.pred.e*domains),
     getValues(jan.tmax.pred.e*domains),
     pch='.')

# WINTER FULL MODEL
mdat <- cmonth[cmonth$ym=='2012 05',1:19] 
topovars <- extract(maystack,cbind(mdat$UTM.east,mdat$UTM.north),
                    method='bilinear')
head(topovars)
mdat <- cbind(mdat,topovars)

fit <- lm(Tmax.mean ~ dem30m + month_rad_tot + tpi500.30m
          + plow500.30m + slp30m
          + dist2falsebay,data=mdat)
summary(fit)
may.tmax.pred <- predict(maystack,fit)
plot(may.tmax.pred*domains)

fit <- lm(Tmin.mean ~ dem30m + month_rad_tot + tpi500.30m
          + plow500.30m + slp30m
          + dist2falsebay,data=mdat)
summary(fit)
may.tmin.pred <- predict(maystack,fit)
plot(may.tmin.pred*domains)
plot(getValues(may.tmin.pred*domains),
     getValues(may.tmax.pred*domains),
     pch='.')

plot(getValues(may.tmin.pred*domains),
     getValues(jan.tmax.pred*domains),pch='.')

# WINTER ELEV ONLY MODEL
fite <- lm(Tmax.mean ~ dem30m 
          + dist2falsebay,data=mdat)
summary(fite)
may.tmax.pred.e <- predict(maystack,fite)
plot(may.tmax.pred.e*domains)

fite <- lm(Tmin.mean ~ dem30m
          + dist2falsebay,data=mdat)
summary(fite)
may.tmin.pred.e <- predict(maystack,fite)
plot(may.tmin.pred.e*domains)

plot(getValues(may.tmin.pred.e*domains),
     getValues(may.tmax.pred.e*domains),
     pch='.')

plot(getValues(may.tmin.pred.e*domains),
     getValues(jan.tmax.pred.e*domains),
     pch='.')

## STACK RESULTS
PRED <- stack(may.tmax.pred,jan.tmax.pred,may.tmin.pred,jan.tmin.pred)*domains
PRED@layernames <- c('Winter Tmax','Summer Tmax','Winter Tmin','Summer Tmin')
plot(PRED)

setwd('/Users/david/Documents/Projects/Dropbox/SouthAfricaResearch/Microclimates/Field_data_2012/analysis/figures')
png('PredictedLayers.png',800,700)
plot(PRED,axes=T,cex=2)
dev.off()

png('PredictedMinTopoElev.png',800,600)
op=par(mfrow=c(1,2))
plot(may.tmin.pred*domains,axes=F,cex=2)
plot(may.tmin.pred.e*domains,axes=F,cex=2)
par(op)
dev.off()

png('PredictedMaxTopoElev.png',800,600)
op=par(mfrow=c(1,2))
plot(jan.tmax.pred*domains,axes=F,cex=2)
plot(jan.tmax.pred.e*domains,axes=F,cex=2)
par(op)
dev.off()


## Plot predicted min vs. max temperatures
wintermin <- getValues(may.tmin.pred*domains)
wintermax <- getValues(may.tmax.pred*domains)
summermin <- getValues(jan.tmin.pred*domains)
summermax <- getValues(jan.tmax.pred*domains)

## Plot predicted min vs. max temperatures - ELEV model
wintermine <- getValues(may.tmin.pred.e*domains)
wintermaxe <- getValues(may.tmax.pred.e*domains)
summermine <- getValues(jan.tmin.pred.e*domains)
summermaxe <- getValues(jan.tmax.pred.e*domains)


png('WinterminXSummermax.png',700,500)
op=par(mfrow=c(1,2))
plot(wintermin,summermax,pch=19,cex=0.2,
     col='#FF000022',
     xlab='Mean winter Tmin (°C)',
     ylab='Mean summer Tmax (°C)',
     cex.lab=1.5)
R=cor(wintermin,summermax,use='pair')
text(7,32,paste('R =',round(R,2)),cex=1.5)

plot(wintermine,summermaxe,pch=19,cex=0.2,
     col='#FF000022',
     xlab='Mean winter Tmin (°C)',
     ylab='Mean summer Tmax (°C)',
     cex.lab=1.5)
R=cor(wintermine,summermaxe,use='pair')
text(8,30,paste('R =',round(R,2)),cex=1.5)
par(op)
dev.off()


plot(summermin,summermax,pch=19,cex=0.2,col='#FF000022',asp=0.5)
cor(summermin,summermax,use='pair')

plot(summermax,wintermax,pch=19,cex=0.2,col='#FF000022',asp=0.5)
cor(summermax,wintermax,use='pair')

plot(summermin,wintermin,pch=19,cex=0.2,col='#FF000022',asp=0.5)
cor(summermin,wintermin,use='pair')
par(op)

## read in Leustr points
spp <- read.csv('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/distributions/tm4maxent.csv')
ls <- spp[spp$species=='Leucadendron strobilinum',]
head(ls)

plots <- read.csv('/Users/david/Documents/Projects/Dropbox/SouthAfricaResearch/VegSurvey/data/datafiles/plots.csv')
head(plots)

png('JanTmax_plots.png',700,700)
plot(PRED[[1]],axes=F)
points(plots$easting,plots$northing,pch=19)
#points(ls$easting,ls$northing,pch=19,col='red')
dev.off()

png('JanTmax_Ls.png',700,700)
plot(PRED[[1]],axes=F)
points(plots$easting,plots$northing,pch=19,cex=0.5)
points(ls$easting,ls$northing,pch=19,col='red')
dev.off()

## export layers for maxent
setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/ascJM4')
writeRaster(jan.tmax.pred*domains,'JanTmax.asc',overwrite=T)
writeRaster(jan.tmin.pred*domains,'JanTmin.asc',overwrite=T)
writeRaster(may.tmax.pred*domains,'MayTmax.asc',overwrite=T)
writeRaster(may.tmin.pred*domains,'MayTmin.asc',overwrite=T)

setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/ascJM4T05')
writeRaster(jan.tmax.pred*domains+0.5,'JanTmax.asc',overwrite=T)
writeRaster(jan.tmin.pred*domains+0.5,'JanTmin.asc',overwrite=T)
writeRaster(may.tmax.pred*domains+0.5,'MayTmax.asc',overwrite=T)
writeRaster(may.tmin.pred*domains+0.5,'MayTmin.asc',overwrite=T)

setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/ascJM4T10')
writeRaster(jan.tmax.pred*domains+1,'JanTmax.asc',overwrite=T)
writeRaster(jan.tmin.pred*domains+1,'JanTmin.asc',overwrite=T)
writeRaster(may.tmax.pred*domains+1,'MayTmax.asc',overwrite=T)
writeRaster(may.tmin.pred*domains+1,'MayTmin.asc',overwrite=T)

setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/ascJM4T15')
writeRaster(jan.tmax.pred*domains+1.5,'JanTmax.asc',overwrite=T)
writeRaster(jan.tmin.pred*domains+1.5,'JanTmin.asc',overwrite=T)
writeRaster(may.tmax.pred*domains+1.5,'MayTmax.asc',overwrite=T)
writeRaster(may.tmin.pred*domains+1.5,'MayTmin.asc',overwrite=T)

setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/ascJM4T20')
writeRaster(jan.tmax.pred*domains+2,'JanTmax.asc',overwrite=T)
writeRaster(jan.tmin.pred*domains+2,'JanTmin.asc',overwrite=T)
writeRaster(may.tmax.pred*domains+2,'MayTmax.asc',overwrite=T)
writeRaster(may.tmin.pred*domains+2,'MayTmin.asc',overwrite=T)

setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/ascJM4T25')
writeRaster(jan.tmax.pred*domains+2.5,'JanTmax.asc',overwrite=T)
writeRaster(jan.tmin.pred*domains+2.5,'JanTmin.asc',overwrite=T)
writeRaster(may.tmax.pred*domains+2.5,'MayTmax.asc',overwrite=T)
writeRaster(may.tmin.pred*domains+2.5,'MayTmin.asc',overwrite=T)

setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/ascJM4T30')
writeRaster(jan.tmax.pred*domains+3,'JanTmax.asc',overwrite=T)
writeRaster(jan.tmin.pred*domains+3,'JanTmin.asc',overwrite=T)
writeRaster(may.tmax.pred*domains+3,'MayTmax.asc',overwrite=T)
writeRaster(may.tmin.pred*domains+3,'MayTmin.asc',overwrite=T)

## WRITE OUT ELEVATION ONLY MODELS
setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/ascJM4E')
writeRaster(jan.tmax.pred.e*domains,'JanTmax.asc',overwrite=T)
writeRaster(jan.tmin.pred.e*domains,'JanTmin.asc',overwrite=T)
writeRaster(may.tmax.pred.e*domains,'MayTmax.asc',overwrite=T)
writeRaster(may.tmin.pred.e*domains,'MayTmin.asc',overwrite=T)

setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/ascJM4ET05')
writeRaster(jan.tmax.pred.e*domains+0.5,'JanTmax.asc',overwrite=T)
writeRaster(jan.tmin.pred.e*domains+0.5,'JanTmin.asc',overwrite=T)
writeRaster(may.tmax.pred.e*domains+0.5,'MayTmax.asc',overwrite=T)
writeRaster(may.tmin.pred.e*domains+0.5,'MayTmin.asc',overwrite=T)

setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/ascJM4ET10')
writeRaster(jan.tmax.pred.e*domains+1,'JanTmax.asc',overwrite=T)
writeRaster(jan.tmin.pred.e*domains+1,'JanTmin.asc',overwrite=T)
writeRaster(may.tmax.pred.e*domains+1,'MayTmax.asc',overwrite=T)
writeRaster(may.tmin.pred.e*domains+1,'MayTmin.asc',overwrite=T)

setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/ascJM4ET15')
writeRaster(jan.tmax.pred.e*domains+1.5,'JanTmax.asc',overwrite=T)
writeRaster(jan.tmin.pred.e*domains+1.5,'JanTmin.asc',overwrite=T)
writeRaster(may.tmax.pred.e*domains+1.5,'MayTmax.asc',overwrite=T)
writeRaster(may.tmin.pred.e*domains+1.5,'MayTmin.asc',overwrite=T)

setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/ascJM4ET20')
writeRaster(jan.tmax.pred.e*domains+2,'JanTmax.asc',overwrite=T)
writeRaster(jan.tmin.pred.e*domains+2,'JanTmin.asc',overwrite=T)
writeRaster(may.tmax.pred.e*domains+2,'MayTmax.asc',overwrite=T)
writeRaster(may.tmin.pred.e*domains+2,'MayTmin.asc',overwrite=T)

## Extract predicted values for all plots
predtemp <- extract(PRED,cbind(plots$easting,plots$northing))
head(predtemp)
plots <- cbind(plots,predtemp)
names(plots)[27:30] <- c('WinterTmax','SummerTmax','WinterTmin','SummerTmin')
head(plots)

plot(plots$WinterTmin,plots$SummerTmax,pch=19)
lsplots <- plots$ID %in% ls$plot
points(plots$WinterTmin[lsplots],plots$SummerTmax[lsplots],pch=19,col='red')

## plotting table mountain summit
plot(PRED[[1]])
tablex <- extent(PRED)
tablex@xmin <- 258500
tablex@xmax <- 264660
tablex@ymin <- 6236800
tablex@ymax <- 6240660
plot(crop(PRED,tablex))

may.tmin.table <- crop(may.tmin.pred,tablex)
jan.tmax.table <- crop(jan.tmax.pred,tablex)

may.tmin.table.10 <- may.tmin.table
may.tmin.table.10[may.tmin.table.10<=10]=1
may.tmin.table.10[may.tmin.table.10>10]=0

plot(may.tmin.table)
plot(jan.tmax.table)
plot(may.tmin.table.10,col=c('#FFFFFF','#FF000022'),add=T,legend=F)

## Ls suitability layer
lsmax <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutJM4T05/Leucadendron_strobilinum.asc')
lsmax05 <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutJM4T05/Leucadendron_strobilinum_ascJM4T05.asc')

lsmax10 <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutJM4T10/Leucadendron_strobilinum_ascJM4T10.asc')

lsmax15 <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutJM4T15/Leucadendron_strobilinum_ascJM4T15.asc')

lsmax20 <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutJM4T20/Leucadendron_strobilinum_ascJM4T20.asc')

lsmaxAll <- stack(lsmax,lsmax05,lsmax10,lsmax15,lsmax20)
plot(crop(lsmaxAll,tablex))

# make hillshade
slp <- terrain(dem30m,'slope')
asp <- terrain(dem30m,'aspect')
hs <- hillShade(slp,asp,45,45)

mb <- c(262129,6238538)

## plot tmin and tmax on HS for summit
plot(crop(hs,tablex),col=gray(1:100/100),legend=F,axes=F)
plot(may.tmin.table,add=T,col=rev(terrain.colors(25,0.6)))
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)

## PLOT 5 SUITABILITY SURFACES FOR TABLE MT TOP
setwd('/Users/david/Documents/Projects/Dropbox/SouthAfricaResearch/Microclimates/Field_data_2012/analysis/figures')

pdf('Leustr_maxent_2deg.pdf',8,5.6)
suit.max <- max(getValues(lsmax),na.rm=T)
plot(crop(hs,tablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(lsmax,tablex),add=T,col=terrain.colors(100,alpha=0.6)[1:round(suit.max*100,0)],axes=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)


suit.max <- max(getValues(lsmax05),na.rm=T)
plot(crop(hs,tablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(lsmax05,tablex),add=T,col=terrain.colors(100,alpha=0.5)[1:round(suit.max*100,0)],axes=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)

suit.max <- max(getValues(lsmax10),na.rm=T)
plot(crop(hs,tablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(lsmax10,tablex),add=T,col=terrain.colors(100,alpha=0.5)[1:round(suit.max*100,0)],axes=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)

suit.max <- max(getValues(lsmax15),na.rm=T)
plot(crop(hs,tablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(lsmax15,tablex),add=T,col=terrain.colors(100,alpha=0.5)[1:round(suit.max*100,0)],axes=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)

suit.max <- max(getValues(lsmax20),na.rm=T)
plot(crop(hs,tablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(lsmax20,tablex),add=T,col=terrain.colors(100,alpha=0.5)[1:round(suit.max*100,0)],axes=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)
dev.off()

pdf('Leustr_maxent_2deg_scalebar.pdf',8,5.6)
plot(crop(hs,tablex),col=gray(1:100/100),legend=F)
dev.off()

#### maxent models using only may tmin and jan tmax as predictors, comparing full topoclimate model to elevation/regional model
setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/')
dir()
ascin <- paste('ascJM4',c('T05','T10','T15','T20','T25','T30'),sep='')
topo <- paste('maxentOutMinMax',c('T05','T10','T15','T20','T25','T30'),sep='')


setwd(topo[1])
suit00 <- raster('Leucadendron_strobilinum.asc')
plot(suit00)
thresh=0.328
pOcc <- length(which(getValues(suit00)>thresh))
pOcc
res <- c(0,pOcc)
tinc <- c(0.5,1,1.5,2,2.5,3)

for (i in 1:6) {
    setwd('../')
    setwd(topo[i])
    futr <- paste('Leucadendron_strobilinum_',ascin[i],'.asc',sep='')
    fsuit <- raster(futr)
    pOcc <- length(which(getValues(fsuit)>thresh))
    res <- rbind(res,c(tinc[i],pOcc))
}
res

# make some maps of topoclimate-based model
setwd('/Users/david/Documents/Projects/Dropbox/SouthAfricaResearch/Microclimates/Field_data_2012/analysis/figures')

png('Leucadendron_strobilinum_minmax_maxent.png',600,600)
plot(hs*domains,col=gray(1:100/100),axes=F,legend=F)
plot(suit00,add=T,col=rev(terrain.colors(25,0.7)),axes=F)
points(plots$easting,plots$northing)
points(ls$easting,ls$northing,pch=19,col='red')
dev.off()

#ELEV ONLY MODEL
setwd('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/')
dir()

ascin <- paste('ascJM4E',c('T05','T10','T15','T20'),sep='')
topo <- paste('maxentOutMinMaxE',c('T05','T10','T15','T20'),sep='')
setwd(topo[1])
suit00 <- raster('Leucadendron_strobilinum.asc')
plot(suit00)
thresh=0.327
pOcc <- length(which(getValues(suit00)>thresh))
pOcc
resE <- c(0,pOcc)
tinc <- c(0.5,1,1.5,2)

for (i in 1:4) {
    setwd('../')
    setwd(topo[i])
    futr <- paste('Leucadendron_strobilinum_',ascin[i],'.asc',sep='')
    fsuit <- raster(futr)
    pOcc <- length(which(getValues(fsuit)>thresh))
    resE <- rbind(resE,c(tinc[i],pOcc))
}
resE

# combine results
LsRangeDecline <- data.frame(cbind(res,c(resE[,2],0,0)))
names(LsRangeDecline) <- c('deltaT','TopoModelRange','ElevModelRange')
LsRangeDecline$TopoModelRelRange <- LsRangeDecline$TopoModelRange/LsRangeDecline$TopoModelRange[1]
LsRangeDecline$ElevModelRelRange <- LsRangeDecline$ElevModelRange/LsRangeDecline$ElevModelRange[1]
LsRangeDecline

setwd('/Users/david/Documents/Projects/Dropbox/SouthAfricaResearch/Microclimates/Field_data_2012/analysis/figures')
png('RangeVTopoElevModels.png',700,500)
plot(TopoModelRelRange~deltaT,
     data=LsRangeDecline,ylim=c(0,1),
     xlab='Temperature Increase (°C)',
     ylab='Relative range size',
     type='b',pch=19,col='blue',cex.lab=1.5)
points(ElevModelRelRange[1:5]~deltaT[1:5],data=LsRangeDecline,type='b',pch=19)
dev.off()

## Plot maxent min/max model at 0,1,2 deg

stablex <- tablex #small extent
stablex@xmin <- 258500
stablex@xmax <- 263600
stablex@ymin <- 6236800
stablex@ymax <- 6240660

pdf('Leustr_maxent_minmax_2deg.pdf',8,5.6)
suit <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutMinMaxT05/Leucadendron_strobilinum.asc')
suit.max <- max(getValues(suit),na.rm=T)
plot(crop(hs,tablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(suit,tablex),add=T,col=terrain.colors(100,alpha=0.6)[1:round(suit.max*100,0)],axes=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)

# get future layersXXXXX
suit <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutMinMaxT10/Leucadendron_strobilinum_ascJM4T10.asc')
suit.max <- max(getValues(suit),na.rm=T)
plot(crop(hs,tablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(suit,tablex),add=T,col=terrain.colors(100,alpha=0.6)[1:round(suit.max*100,0)],axes=F,legend=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)

suit <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutMinMaxT20/Leucadendron_strobilinum_ascJM4T20.asc')
suit.max <- max(getValues(suit),na.rm=T)
plot(crop(hs,tablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(suit,tablex),add=T,col=terrain.colors(100,alpha=0.6)[1:round(suit.max*100,0)],axes=F,legend=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)
dev.off()

pdf('Leustr_maxent_minmaxTvE_2deg.pdf',10,4.55)
par(mfrow=c(1,2))
suit <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutMinMaxT20/Leucadendron_strobilinum.asc')
suit.max <- max(getValues(suit),na.rm=T)
plot(crop(hs,stablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(suit,stablex),add=T,col=terrain.colors(100,alpha=0.6)[1:round(suit.max*100,0)],axes=F,legend=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)

suitE <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutMinMaxET20/Leucadendron_strobilinum.asc')
suit.max <- max(getValues(suitE),na.rm=T)
plot(crop(hs,stablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(suit,stablex),add=T,col=terrain.colors(100,alpha=0.6)[1:round(suit.max*100,0)],axes=F,legend=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)

suit <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutMinMaxT10/Leucadendron_strobilinum_ascJM4T10.asc')
suit.max <- max(getValues(suit),na.rm=T)
plot(crop(hs,stablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(suit,stablex),add=T,col=terrain.colors(100,alpha=0.6)[1:round(suit.max*100,0)],axes=F,legend=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)

suitE <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutMinMaxET10/Leucadendron_strobilinum_ascJM4ET10.asc')
suit.max <- max(getValues(suitE),na.rm=T)
plot(crop(hs,stablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(suit,stablex),add=T,col=terrain.colors(100,alpha=0.6)[1:round(suit.max*100,0)],axes=F,legend=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)

suit <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutMinMaxT20/Leucadendron_strobilinum_ascJM4T20.asc')
suit.max <- max(getValues(suit),na.rm=T)
plot(crop(hs,stablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(suit,stablex),add=T,col=terrain.colors(100,alpha=0.6)[1:round(suit.max*100,0)],axes=F,legend=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)

suitE <- raster('/Users/david/Documents/Projects/SouthAfricaResearch/VegMaxentModeling/maxent/Leustr/JanMay4Model/maxentOutMinMaxET20/Leucadendron_strobilinum_ascJM4ET20.asc')
suit.max <- max(getValues(suitE),na.rm=T)
plot(crop(hs,stablex),col=gray(1:100/100),legend=F,axes=F)
plot(crop(suit,stablex),add=T,col=terrain.colors(100,alpha=0.6)[1:round(suit.max*100,0)],axes=F,legend=F)
points(ls$easting,ls$northing,pch=19)
points(plots$easting,plots$northing)
points(mb[1],mb[2],pch=8,cex=1.5)
par(op)
dev.off()
system('open Leustr_maxent_minmaxTvE_2deg.pdf')

png('TableMtHS.png',400,400)
plot(crop(hs,stablex),col=gray(1:100/100),axes=F,legend=F)
dev.off()