## function that takes daily tmax data and thl algorithm, and finds value of folding axis that maximizes fit for insolation values

# x = r sinq cosf,     y = r sinq sinf,     z = r cosq,

sa2xyz <- function(slp,asp,unit='deg') {
    d2r <- function(d) pi * d/180
    if (unit=='deg') {
        slp <- d2r(slp)
        asp <- d2r(asp)
    }    
    x <- sin(slp) * cos(asp)
    y <- sin(slp) * sin(asp)
    z <- cos(slp)
    return(c(x,y,z))
}

divang <- function(slp1,asp1,slp2,asp2,unit='deg') {
    d2r <- function(d) pi * d/180
    sa2xyz <- function(slp,asp) {
        x <- sin(slp) * cos(asp)
        y <- sin(slp) * sin(asp)
        z <- cos(slp)
        return(c(x,y,z))
    }
    
    if (unit=='deg') {
        slp1 <- d2r(slp1);slp2 <- d2r(slp2)
        asp1 <- d2r(asp1);asp2 <- d2r(asp2)
    }

    xyz1 <- sa2xyz(slp1,asp1)
    xyz2 <- sa2xyz(slp2,asp2)

    dp <- sum(xyz1*xyz2)
    acos(dp)
}

divang(0,90,30,60)

sa2xyz(30,90)

x <- seq(0,2*pi,length.out=100)
plot(x,sin(x))
plot(x,cos(x))

fold <- function(x,masp) abs(pi-abs(x-d2r(masp)))

dang <- function(x,ref) {
    ref <- d2r(ref)
    ref+(ref-x)
}
plot(x,cos(x-d2r(0)))


