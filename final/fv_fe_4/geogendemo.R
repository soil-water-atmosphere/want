library(FVFE2D)

# A dummy flux has to be created because the models created in the geogendemo function cannot be constructed without
# a system flux function. The actual return value of this function is not relevant because is it does not affect the 
# node generation process.
dummyflux = function(x, y, s, grads)
{
  return(-1)
}

# "distance x not equal to distance y"
# "distance increasing in y-direction"
# "distance increasing in x-direction"
# "distance smaller near (5,5)"
# "distance larger near (5,5)"
# "distance order of 1, some prefixed node at lower center"
# "distance order of 0.3"
# "distance smaller near line"
descr = c()

descr[1] = "distance order of 1, graph-verbose"
descr[2] = "distance order of 1, non-grap-verbose"
descr[3] = "XXXX"
descr[4] = "XXXX"
descr[5] = "XXXX"
descr[6] = "XXXX"
descr[7] = "XXXX"
descr[8] = "XXXX"
descr[9] = "XXXX"
descr[10] = "XXXX"

# Generates nodal fields making use of several different algorithms.
geogendemo = function(case=Inf)
{ 
  casedescriptions = list(
    "constant (rather large) distance ",
    "constant (large) distance, non-graphverbose",
    "constant (large) distance, with initial nodes",
    "constant (somewhat smaller) distance",
    "constant distance, ytoxratio = 4",
    "distance increasing in x-direction",  
    "distance increasing in y-direction",
    "distance smaller near (5,5)",
    "distance smaller near (5,5)",
    "distance larger near (5,5)"
  )
  
  METHOD = "FV"
  
  if(case > 10)
  {
    print(paste("demo with case number",case," does not exist"))
  }
  if(case==1)
  {
    domain = matrix(c(1,2,6,1,9,7,4,9),ncol=2,byrow=TRUE)
    model = newFLOW2D(domain,dummyflux)
    set.name(model,descr[1])
    optdist = function(p){return(1.0)}
    set.discretisation(model,
                       nodes=list(type="geomgen",optdist=optdist,verbose=TRUE),method=METHOD)
    plot(model,fill="random")
  } else if(case==2)
  {
    domain = matrix(c(1,2,6,1,9,7,4,9),ncol=2,byrow=TRUE)
    model = newFLOW2D(domain,dummyflux)
    set.name(model,descr[2])
    optdist = function(p){return(1.0)}
    set.discretisation(model,
                       nodes=list(type="geomgen",optdist=optdist,verbose=FALSE),method=METHOD)
    plot(model,fill="random")
  } else if(case==3)
  {
    domain = matrix(c(1,2,6,1,9,7,4,9),ncol=2,byrow=TRUE)
    model = newFLOW2D(domain,dummyflux)
    set.name(model,descr[3])
    optdist = function(p){return(1)}
    startnodes = cbind(rep(5,length=8),seq(2,4,length=8))
    set.discretisation(model,
                       nodes=list(type="geomgen",optdist=optdist,
                                  startnodes = startnodes,
                                  maxiter=20,stopcrit=0.005,
                                  verbose=TRUE,sleeptime=0.1),method=METHOD)
    plot(model,fill="random")
  }else if(case==4)
  {
    domain = matrix(c(1,2,6,1,9,7,4,9),ncol=2,byrow=TRUE)
    model = newFLOW2D(domain,dummyflux)
    set.name(model,descr[4])
    optdist = function(p){return(0.3)}
    set.discretisation(model,
                       nodes=list(type="geomgen",optdist=optdist,
                                  maxiter=20,stopcrit=0.005,
                                  verbose=TRUE,sleeptime=0.05),method=METHOD)
    plot(model,fill="random")
  } else if(case==5)
  {
    domain = matrix(c(0,0,10,0,10,1,5,2,0,1),byrow=TRUE,ncol=2)
    model = newFLOW2D(domain,dummyflux)
    set.name(model,descr[5])
    optdist = function(p){return(0.7)}
    set.discretisation(model,
                       nodes=list(type="geomgen",optdist=optdist,
                                  maxiter=20,stopcrit=0.005,
                                  ytoxratio=4,
                                  verbose=TRUE,sleeptime=0.1),method=METHOD)
    set.name(model,"notice the aspect ratio in the plot!")
    plot(model,fill="random")
  } else if(case==6)
  {
    domain = matrix(c(1,2,6,1,9,7,4,9),ncol=2,byrow=TRUE)
    model = newFLOW2D(domain,dummyflux)
    set.name(model,descr[6])
    optdist = function(p){return(0.2+p[1]/15)}
    set.discretisation(model,
                       nodes=list(type="geomgen",optdist=optdist,
                                  maxiter=20,stopcrit=0.005,
                                  verbose=TRUE),method=METHOD)
    plot(model,fill="random")
  } else if(case==7)
  {
    print(casedescriptions[[case]])
    domain = matrix(c(1,2,6,1,9,7,4,9),ncol=2,byrow=TRUE)
    model = newFLOW2D(domain,dummyflux)
    set.name(model,descr[7])
    optdist = function(p){return(0.2+p[2]/15)}
    set.discretisation(model,
                       nodes=list(type="geomgen",optdist=optdist,
                                  maxiter=20,stopcrit=0.005,
                                  verbose=TRUE,sleeptime=0.05),method=METHOD)
    plot(model,fill="random")
  } else if(case==8)
  {
    domain = matrix(c(1,2,6,1,9,7,4,9),ncol=2,byrow=TRUE)
    model = newFLOW2D(domain,dummyflux)
    set.name(model,descr[8])
    optdist = func.dist.to.point(c(5,5),4,0.2,0.6)
    set.discretisation(model,
                       nodes=list(type="geomgen",optdist=optdist,
                                  maxiter=20,stopcrit=0.005,
                                  verbose=TRUE,sleeptime=0.05),method=METHOD)
    plot(model,fill="random")
  } else if(case==9)
  {
    domain = matrix(c(1,2,6,1,9,7,4,9),ncol=2,byrow=TRUE)
    model = newFLOW2D(domain,dummyflux)
    set.name(model,descr[9])
    optdist = func.dist.to.point(c(5,5),4,0.5,0.3)
    set.discretisation(model,
                       nodes=list(type="geomgen",optdist=optdist,
                                  maxiter=20,stopcrit=0.005,
                                  verbose=TRUE,sleeptime=0.05),method=METHOD)
    plot(model,fill="random")
  }  else if(case==10)
  {
    domain = matrix(c(1,2,6,1,9,7,4,9),ncol=2,byrow=TRUE)
    model = newFLOW2D(domain,dummyflux)
    set.name(model,descr[10])
    line = matrix(c(3,3,5,5,5,7),ncol=2,byrow=TRUE)
    optdist = func.dist.to.line(line,1,0.3,0.6)
    set.discretisation(model,
                       nodes=list(type="geomgen",optdist=optdist,
                                  maxiter=20,stopcrit=0.005,
                                  verbose=TRUE,sleeptime=0.05),method=METHOD)
    plot(model,fill="random")
  } 
}

# Running this return a specific discretization.
geogendemo(1)