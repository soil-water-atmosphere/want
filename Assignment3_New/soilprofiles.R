#rm(list=ls())

# This installs the manipulate package if not installed yet, thereafter the package is loaded.
if(!require(manipulate)) {install.packages("manipulate")
  library(manipulate)}

load(file='Staring.Rdata')

soiltype = list(type=names(soil.set)) #all soil type into a list soiltype
psi.pnt = seq(4,0,length.out=100) # required data points in pressure

psi.axis = seq(4,0,length.out=10)
k.axis = seq(0,50,length.out=10)


doplot = function(soiltype,nr)
{
  old.par = par(no.readonly=TRUE)
  par(mar=c(5,5,5,5))
  plot(soil.set[[soiltype$type[nr]]]$theta.fun(-10^psi.pnt),psi.pnt,
       type='l',lwd=3,col='blue',main = c('soil curves pF(theta) and k(theta)',
                                          soil.set[[soiltype$type[nr]]]$name,
                                          soil.set[[soiltype$type[nr]]]$soiltype),
       axes=F,xlab='',ylab='')
  
  theta.pnt = seq(soil.set[[soiltype$type[nr]]]$theta.res,
                  soil.set[[soiltype$type[nr]]]$theta.sat,length.out=10)
  axis(1)
  mtext(side=1,'moisture content (-)',line=2.5,cex=1.5)
  axis(2)
  mtext(side=2,'pF (-log(psi))',col='blue',line=2.5,cex=1.5)
  grid(col="black")
  par(new=TRUE)
  plot(soil.set[[soiltype$type[nr]]]$theta.fun(-10^psi.pnt),
       soil.set[[soiltype$type[nr]]]$k.fun(-10^psi.pnt),
       xlab="",ylab="",
       col='red',axes=FALSE,
       type="l",lwd=3)
  
  axis(4)
  mtext(side=4,'hydraulic conductivity (cm/d)',col='red',line=2.5,cex=1.5)
  box()
  par(old.par)
}

doplot2 = function(soilchoice)
{
  old.par = par(no.readonly=TRUE)
  par(mar=c(5,5,5,5))
  plot(soil.set[[soilchoice]]$theta.fun(-10^psi.pnt),psi.pnt,
       type='l',lwd=3,col='blue',main = c('soil curves pF(theta) and k(theta)',
                                          soil.set[[soilchoice]]$name,
                                          soil.set[[soilchoice]]$soiltype),
       axes=F,xlab='',ylab='')
  
  theta.pnt = seq(soil.set[[soilchoice]]$theta.res,
                  soil.set[[soilchoice]]$theta.sat,length.out=10)
  
  axis(1)
  mtext(side=1,'moisture content (-)',line=2.5,cex=1.5)
  
  axis(2)
  mtext(side=2,'pF (-log(psi))',col='blue',line=2.5,cex=1.5)
  grid(col="black")
  par(new=TRUE)
  plot(soil.set[[soilchoice]]$theta.fun(-10^psi.pnt),
       soil.set[[soilchoice]]$k.fun(-10^psi.pnt),
       xlab="",ylab="",
       col='red',axes=FALSE,
       type="l",lwd=3)
  
  axis(4)
  mtext(side=4,'hydraulic conductivity (cm/d)',col='red',line=2.5,cex=1.5)
  
  box()
  par(old.par)
}

# you may choose one of the manipulate function
# difference is only the soil 'selector'
#manipulate(doplot(soiltype,nr),nr=slider(1,36))
manipulate(doplot2(soilchoice),soilchoice = picker(as.list(soiltype$type)))

