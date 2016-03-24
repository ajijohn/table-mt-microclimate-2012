## model term partitioning

meta <- read.csv('data/csv_masters/location_meta.csv',as.is=T)
dim(meta)
head(meta)

topo10 <- read.csv('data/csv_masters/topo10.csv',as.is=T)
dim(topo10)
names(topo10)

dw <- read.csv('data/csv_masters/2012daily.csv',as.is=T)
head(dw)

dlySummary <- read.csv('data/csv_outfiles/dlySummary.csv',as.is=T)
head(dlySummary)

dlySol <- readRDS('data/Rdata/dlySol.Rdata')
dlySol[1:6,1:6]

days <- as.character(1:366)
days[nchar(days)==1] <- paste('00',days[nchar(days)==1],sep='')
days[nchar(days)==2] <- paste('0',days[nchar(days)==2],sep='')

radterm <- c('dsol','rad080','rad172','rad355','thl315','thl337','thl0')
hillterm <- c('tpi050','tpi125','tpi250','tpi500','plow050','plow125','plow250','plow500')
regterm <- c('d2fb','d2at','d2cs')
slterm <- 'slope'
nModels <- length(radterm)*length(hillterm)*length(regterm)

modterms <- matrix(NA,nModels,5)
modnums <- matrix(NA,nModels,5)
n <- 0
for (i in 1:length(radterm)) for (j in 1:length(hillterm)) for (k in 1:length(regterm)) {
    n <- n+1
    modterms[n,] <- c('elevation',radterm[i],hillterm[j],regterm[k],slterm)
    modnums[n,] <- c(1,i,j,k,1)
}

vars <- c('Tmin','Tmax','RHsat.hrs','VPmax')
allRes <- readRDS('data/Rdata/M5out.Rdata')

v <- 1
d <- 137
bfmod <- allRes[[7]][[v]][d]
modterms[bfmod,]

dd <- subset(dw,dw$doy==d)
dd <- dd[match(dd$siteID,meta$siteID),]
dd$yvar <- dd[,vars[v]]
dd$yvar[meta$use4Analyses==0] <- NA
topo10$dsol <- dlySol[,paste('rad_tot_',days[d],sep='')]

#  Base model with elevation only
plot(dd$yvar~elevation,data=topo10)
fit1 <- lm(dd$yvar~elevation,data=topo10)
RSQelev <- summary(fit1)$r.sq
MSEelev <- sd(fit1$residuals,na.rm=T)

i <- modnums[bfmod,2]
j <- modnums[bfmod,3]
k <- modnums[bfmod,4]
c(i,j,k)

plot(dd$yvar~topo10[,radterm[i]])
plot(dd$yvar~topo10[,hillterm[j]])
plot(dd$yvar~topo10[,regterm[k]])
plot(dd$yvar~topo10[,slterm])

fit2 <- glm(dd$yvar~topo10[,'elevation']+topo10[,radterm[i]]+topo10[,hillterm[j]]+topo10[,regterm[k]]+topo10[,slterm])
summary(fit2)
drop1(fit2)

RSQm5 <- summary(fit2)$r.sq
MSEm5 <- sd(fit2$residuals,na.rm=T)

drop1(fit2)$AIC[-1]
attributes(drop1(fit2))
