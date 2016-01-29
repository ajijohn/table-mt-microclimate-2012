## Code for topographic exposure - carefully checked for southern hemisphere, and folded aspect calculations
## D. Ackerly, Jan. 28, 2016

## Topographic exposure models
r2d <- function(r) 180 * r/pi
d2r <- function(d) pi * d/180

## northness, eastness
facing <- function(slope,aspect,focal=180,unit='rad') {
    if (unit %in% c('rad','deg')) {
        if (unit=='deg') {
            slope <- d2r(slope)
            aspect <- d2r(aspect)
        }
        aspect <- d2r(focal) - aspect
        return(sin(slope) * cos(aspect))
    } else print('unit must be rad or deg')
}

eastness <- function(slope,aspect,unit='deg') facing(slope,aspect,focal=90,unit=unit)
northness <- function(slope,aspect,unit='deg') facing(slope,aspect,focal=0,unit=unit)

#Topographic Heat Load
# thl <- function(lat,slp,asp,unit='rad',masp=225) {
#     d2r <- function(x) pi * x/180
#     folded <- function(x,masp) pi-abs(x-d2r(masp))
#     if (unit=='deg') {
#         L <- d2r(L)
#         A <- d2r(A)
#         S <- d2r(S)
#     }
#     asp <- folded(asp)
#     return(0.339+0.808*cos(L)*cos(S)-0.196*sin(L)*sin(S)-0.482*cos(A)*sin(S))
# }
# 

# thl - 
#source('~/Documents/Projects/Toolbox/insolation/insolation_regressions.R', echo=F)
#insolation(lat=45,slope=30,asp = 0,units = 'deg')

# Solar insolation index - Gustafson et al. 2003
#2 - sin((slope/90)180) * (cos(22-aspect)+1)

# sii <- function(slope,aspect,slope.units='degree',asp.units='degree') {
#     if (slope.units=='degree') slope <- pi * slope/180
#     #if (slope.units=='percent') slope <- 
#     if (asp.units=='degree') aspect <- pi * aspect/180
#     #2 - sin((slope/90)180) * (cos(22-aspect)+1) - from Batchelet book
#     return(2-sin(2*slope) * cos(pi*22/180-aspect)+1)
# }

# McCune and Keon 2002 JVS
THL = function(lat=45,slp=0,asp=0,eqn=3,fold.axis=225,unit='deg',rad.unit='yr')
{   
    d2r <- function(d) pi * d/180
    r2d <- function(r) 180 * r/pi
    
    if (unit=='deg')
    {
        lat = d2r(lat)
        slp = d2r(slp)
        asp = d2r(asp)
    }
    
    if (abs(lat) > d2r(60)) print('latitude exceeds 60°; equations invalid')
    if (slp > d2r(90)) print('slope exceeds 90°; check data!')
    
    if ( (abs(lat)>=d2r(30)) & (slp<=d2r(60)) ) eqn <- 3
    if ( (abs(lat)<d2r(30)) & (slp<=d2r(60)) ) eqn <- 2
    if ( (abs(lat)<d2r(30)) & (slp>d2r(60)) ) eqn <- 1
    
#     if (eqn==2)
#         if (slp>d2r(60)) 
#             print('slope exceeds range for eqn 2')
#     if (eqn==3) 
#     {
#         if (slp>d2r(60)) 
#             print('slope exceeds range for eqn 3')
#         if (abs(lat)<d2r(30))
#             print('latitude exceeds range for eqn 3')
#         if (abs(lat)>d2r(60))
#             print('latitude exceeds range for eqn 3')
#     }
    
    if (lat<0) {
        lat <- abs(lat)
        asp = pi-asp
        if (asp<0) asp <- asp + 2*pi
        fold.axis <- 270+(270-fold.axis)
    }
    
    asp = abs(pi - abs(asp-d2r(fold.axis)))
    
    con = c(-1.467,-1.236,0.339)
    a1 = c(1.582,1.35,0.808)
    a2 = c(-1.5,-1.376,0)
    a3 = c(-0.262,-0.331,-0.196)
    a4 = c(0.607,0.375,0)
    a5 = c(0,0,-0.482)
    coeff = rbind(con,a1,a2,a3,a4,a5)
    
    b = rep(0,6)
    b[1] = 1
    b[2] = cos(lat) * cos(slp)
    b[3] = cos(asp) * sin(slp) * sin(lat)
    b[4] = sin(lat) * sin(slp)
    b[5] = sin(asp) * sin(slp)
    b[6] = cos(asp) * sin(slp)
    
    ins = sum(coeff[,eqn] * b)
    if (eqn %in% c(1,2)) ins = exp(ins)
    if (rad.unit=='day') ins = 10000*ins/365
    return(ins)
}