#' Construct a new AFLOW2D model
#' @description generates a new object of the class AFLOW2D:
#' the base of a two dimensional flow
#' @param domain a matrix with two columns containing respectively the
#' x and y coordinates of the cornerpoints that define the polygonal domain
#' on which the model is to be solved
#' @param systemfluxfunction a function of five arugments:
#' coordinates  x and y defining a point withtin the domain,
#' the state s at this point and the
#' gradients dsdx and dsdy of the state at that point; it has to return a
#' vector of two number that
#' gives the flux in the x and y direction respecitvely
#' based on these five arguments
#' @param name an optional name of the new object
#' @return an object of the class AFLOW2D
#' @export
#' @examples
#' kvalues = data.frame(x=c(3,6),y=c(2,7),k=c(3,10))
#' kfun = nearestneighbour.fun(kvalues,zname="k")
#' invgradflux = function(x,y,s,gradx,grady)
#' {
#' return(-kfun(x,y)*s*c(gradx,0.2*grady))
#' }
#' domain = matrix(c(1,2,6,1,9,7,4,9),ncol=2,byrow=TRUE)
#' M = newAFLOW2D(domain=domain,systemfluxfunction=invgradflux,name="example")
#' summary(M)
newAFLOW2D = function(domain=NULL,
                     systemfluxfunction=NULL,
                     name="")
{
  is.valid("m","+x2",domain,
           "first (domain) argument of newAFLOW2D should be a matrix with two columns")
  coll = test.collinearity(domain)
  if(nrow(coll)>0)
  {
    cat("There do exist collinear points in the boundary of the domain.\n")
    cat("This may generate problems during node generation.\n")
    cat("You may consider to remove this collinearity by the jitter.points function.\n")
    cat("List of collinear points:\n")
    for(i in 1:nrow(coll))
    {
      cat(paste("points",coll[i,1],coll[i,2],coll[i,3],"\n"))
    }
  }
  is.valid("g",5,systemfluxfunction,
           "second (systemfluxfunction) argument of newAFLOW2D should be a function with five
  formal arguments")
  is.valid("s","*",name,
           "third (name) argument of newAFLOW2D should be a string")


  result = new.env()

  if(name=="")
  {
    result$name=paste("AFLOW2D",gsub(" ","_",date()),sep="")
  } else {
    result$name = name
  }
  result$domain = domain

  result$xrange = range(domain[,1])
  result$yrange = range(domain[,2])
  result$xmin = result$xrange[1]
  result$ymin = result$yrange[1]



  result$doscale = function(P)
  {
    if(is.null(dim(P)))
    {
      c((P[1]-result$xmin)/result$xscale,(P[2]-result$ymin)/result$yscale)
    } else {
      t(apply(P,1,function(r){c((r[1]-result$xmin)/result$xscale,
                                (r[2]-result$ymin)/result$yscale)}))
    }
  }

  result$dounscale = function(P)
  {
    if(is.null(dim(P)))
    {
      c(result$xmin+P[1]*result$xscale,result$ymin+P[2]*result$yscale)
    } else {
      t(apply(P,1,function(r){c(result$xmin+r[1]*result$xscale,
                                result$ymin+r[2]*result$yscale)}))
    }
  }

  setytoxratiowithscales(result,1.0)
  result$sydomain = result$doscale(result$domain)


  result$systemflux = Vectorize(systemfluxfunction)
  result$sdepspatxflux = list()
  result$sindepspatxflux = list()
  result$sdeppntxflux = list()
  result$sindeppntxflux = list()
  result$linexflux= list()

  result$isacceptable = alwaysacceptable2D
  # calculating some geometrical interesting quantities
  N = dim(domain)[1]

  D = (domain[N,1]*domain[1,2]-domain[1,1]*domain[N,2])
  area = D/2
  centerx = (domain[N,1]+domain[1,1])*D
  centery = (domain[N,2]+domain[1,2])*D
  for(i in 1:(N-1))
  {
    D = (domain[i,1]*domain[i+1,2]-domain[i+1,1]*domain[i,2])
    area = area + D/2
    centerx = centerx + (domain[i,1]+domain[i+1,1])*D
    centery = centery + (domain[i,2]+domain[i+1,2])*D
  }
  result$area = abs(area)
  result$center=c(centerx/(6*area),centery/(6*area))
  # creating boundaries
  result$BC = list()
  centerx = mean(domain[,1])
  centery = mean(domain[,2])
  for(i in 1:N)
  {
    beginp = domain[i,]
    if(i<N) endp = domain[i+1,] else endp=domain[1,]
    midp=(beginp+endp)/2
    name = winddirname(midp[1]-result$center[1],result$ytoxratio*(midp[2]-result$center[2]))
    normal = endp-beginp
    lnormal = sqrt(normal[1]^2+normal[2]^2)
    normal = c(-normal[2]/lnormal,normal[1]/lnormal)
    if(!GEO$inside.poly(result$sydomain,result$doscale(midp)+1e-6*4*normal))
    {
      normal = -normal
    }
    result$BC[[i]] = list(beginp = beginp, endp = endp,
                          normal = normal,name=name,
                          type="fixedflux",func=zeroflux)
  }

  attr(result,"class")="AFLOW2D"
  return(result)
}

#' @export add.lineflux.AFLOW2D
add.lineflux.AFLOW2D = add.lineflux.FLOW2D

#' @export add.spatialflux.AFLOW2D
add.spatialflux.AFLOW2D = add.spatialflux.FLOW2D


#' @export add.pointflux.AFLOW2D
add.pointflux.AFLOW2D = add.pointflux.FLOW2D

#' @export set.name.AFLOW2D
set.name.AFLOW2D = set.name.FLOW2D


#' @export plot.BC.id.AFLOW2D
plot.BC.id.AFLOW2D = plot.BC.id.FLOW2D


#' @export set.BC.fixedstate.AFLOW2D
set.BC.fixedstate.AFLOW2D = set.BC.fixedstate.FLOW2D

#' @export set.BC.fixedflux.AFLOW2D
set.BC.fixedflux.AFLOW2D = set.BC.fixedflux.FLOW2D

#' @export set.BC.fixedvectorflux.AFLOW2D
set.BC.fixedvectorflux.AFLOW2D = set.BC.fixedvectorflux.FLOW2D

#' @export set.BC.fluxstate.AFLOW2D
set.BC.fluxstate.AFLOW2D = set.BC.fluxstate.FLOW2D

#' @export set.BC.vectorfluxstate.AFLOW2D
set.BC.vectorfluxstate.AFLOW2D = set.BC.vectorfluxstate.FLOW2D

#' @export set.isacceptable.AFLOW2D
set.isacceptable.AFLOW2D = set.isacceptable.FLOW2D


# #
# # end of mathematical model building
# #
#
# #
# # start of numerical model building
# #
#


#' @export set.isacceptable.AFElinear2D
set.isacceptable.AFElinear2D = set.isacceptable.FLOW2D

#' @export set.isacceptable.FEquadratic2D
set.isacceptable.AFEquadratic2D = set.isacceptable.FLOW2D

#' @export set.isacceptable.FEcubic2D
set.isacceptable.AFEcubic2D = set.isacceptable.FLOW2D

#' @export set.isacceptable.FEquartic2D
set.isacceptable.AFEquartic2D = set.isacceptable.FLOW2D



#' @export set.discretisation.AFLOW2D
set.discretisation.AFLOW2D = function(model,nodes,method)
{
  if(method=="FV")
  {
    cat("Method FV does not apply to anisotrpic flows\n")
    cat("No discretisation set\n")
    return(NULL)
  }
  set.discretisation.FLOW2D(model,nodes,method)
  attr(model,"class") = paste("A",class(model),sep="")
}



#
#numerical boundary condition
#



do.numericalbc.AFElinear2D = do.numericalbc.GENFE2D
do.numericalbc.AFEquadratic2D = do.numericalbc.GENFE2D
do.numericalbc.AFEcubic2D = do.numericalbc.GENFE2D
do.numericalbc.AFEquartic2D = do.numericalbc.GENFE2D


#' @export plot.BC.id.AFElinear2D
plot.BC.id.AFElinear2D = plot.BC.id.FLOW2D

#' @export plot.BC.id.AFEquadratic2D
plot.BC.id.AFEquadratic2D = plot.BC.id.FLOW2D

#' @export plot.BC.id.AFEcubic2D
plot.BC.id.AFEcubic2D = plot.BC.id.FLOW2D

#' @export plot.BC.id.AFEquartic2D
plot.BC.id.AFEquartic2D = plot.BC.id.FLOW2D


#' @export set.BC.fixedstate.AFElinear2D
set.BC.fixedstate.AFElinear2D = set.BC.fixedstate.GENFVFE2D

#' @export set.BC.fixedstate.AFEquadratic2D
set.BC.fixedstate.AFEquadratic2D = set.BC.fixedstate.GENFVFE2D

#' @export set.BC.fixedstate.AFEcubic2D
set.BC.fixedstate.AFEcubic2D = set.BC.fixedstate.GENFVFE2D

#' @export set.BC.fixedstate.AFEquartic2D
set.BC.fixedstate.AFEquartic2D = set.BC.fixedstate.GENFVFE2D


#' @export set.BC.fixedflux.AFElinear2D
set.BC.fixedflux.AFElinear2D = set.BC.fixedflux.GENFVFE2D

#' @export set.BC.fixedflux.AFEquadratic2D
set.BC.fixedflux.AFEquadratic2D = set.BC.fixedflux.GENFVFE2D

#' @export set.BC.fixedflux.AFEcubic2D
set.BC.fixedflux.AFEcubic2D = set.BC.fixedflux.GENFVFE2D

#' @export set.BC.fixedflux.AFEquartic2D
set.BC.fixedflux.AFEquartic2D = set.BC.fixedflux.GENFVFE2D


#' @export set.BC.fluxstate.AFElinear2D
set.BC.fluxstate.AFElinear2D = set.BC.fluxstate.GENFVFE2D

#' @export set.BC.fluxstate.AFEquadratic2D
set.BC.fluxstate.AFEquadratic2D = set.BC.fluxstate.GENFVFE2D

#' @export set.BC.fluxstate.AFEcubic2D
set.BC.fluxstate.AFEcubic2D = set.BC.fluxstate.GENFVFE2D

#' @export set.BC.fluxstate.AFEquartic2D
set.BC.fluxstate.AFEquartic2D = set.BC.fluxstate.GENFVFE2D


#' @export set.BC.fixedvectorflux.AFElinear2D
set.BC.fixedvectorflux.AFElinear2D = set.BC.fixedvectorflux.GENFVFE2D

#' @export set.BC.fixedvectorflux.AFEquadratic2D
set.BC.fixedvectorflux.AFEquadratic2D = set.BC.fixedvectorflux.GENFVFE2D

#' @export set.BC.fixedvectorflux.AFEcubic2D
set.BC.fixedvectorflux.AFEcubic2D = set.BC.fixedvectorflux.GENFVFE2D

#' @export set.BC.fixedvectorflux.AFEquartic2D
set.BC.fixedvectorflux.AFEquartic2D = set.BC.fixedvectorflux.GENFVFE2D



#' @export set.BC.vectorfluxstate.AFElinear2D
set.BC.vectorfluxstate.AFElinear2D = set.BC.vectorfluxstate.GENFVFE2D

#' @export set.BC.vectorfluxstate.AFEquadratic2D
set.BC.vectorfluxstate.AFEquadratic2D = set.BC.vectorfluxstate.GENFVFE2D

#' @export set.BC.vectorfluxstate.AFEcubic2D
set.BC.vectorfluxstate.AFEcubic2D = set.BC.vectorfluxstate.GENFVFE2D

#' @export set.BC.vectorfluxstate.AFEquartic2D
set.BC.vectorfluxstate.AFEquartic2D = set.BC.vectorfluxstate.GENFVFE2D

#
#  allow also new spatialfluxes in FE FV part
#

#' @export add.spatialflux.AFElinear2D
add.spatialflux.AFElinear2D = add.spatialflux.FLOW2D

#' @export add.spatialflux.AFEquadratic2D
add.spatialflux.AFEquadratic2D = add.spatialflux.FLOW2D

#' @export add.spatialflux.AFEcubic2D
add.spatialflux.AFEcubic2D = add.spatialflux.FLOW2D

#' @export add.spatialflux.AFEquartic2D
add.spatialflux.AFEquartic2D = add.spatialflux.FLOW2D

#
#
# numerical pointflux localisation
#


#
do.numericalpointfluxes.AFElinear2D = do.numericalpointfluxes.GENFE2D
do.numericalpointfluxes.AFEquadratic2D = do.numericalpointfluxes.GENFE2D
do.numericalpointfluxes.AFEcubic2D = do.numericalpointfluxes.GENFE2D
do.numericalpointfluxes.AFEquartic2D = do.numericalpointfluxes.GENFE2D
#


#
#  allow to add point fluxes to numerical models
#

#' @export add.pointflux.AFElinear2D
add.pointflux.AFElinear2D = add.pointflux.GENFVFE2D

#' @export add.pointflux.AFEquadratic2D
add.pointflux.AFEquadratic2D = add.pointflux.GENFVFE2D

#' @export add.pointflux.AFEcubic2D
add.pointflux.AFEcubic2D = add.pointflux.GENFVFE2D

#' @export add.pointflux.AFEquartic2D
add.pointflux.AFEquartic2D = add.pointflux.GENFVFE2D

# initialisation

#' @export do.initialize.AFLOW2D
do.initialize.AFLOW2D = do.initialize.FLOW2D

#' @export do.initialize.AFElinear2D
do.initialize.AFElinear2D = do.initialize.GENFVFE2D

#' @export do.initialize.AFEquadratic2D
do.initialize.AFEquadratic2D = do.initialize.GENFVFE2D

#' @export do.initialize.AFEcubic2D
do.initialize.AFEcubic2D = do.initialize.GENFVFE2D

#' @export do.initialize.AFEquartic2D
do.initialize.AFEquartic2D = do.initialize.GENFVFE2D

#
# checking for acceptability

statesareacceptable.AFElinear2D = statesareacceptable.GENFE2D
statesareacceptable.AFEquadratic2D = statesareacceptable.GENFE2D
statesareacceptable.AFEcubic2D = statesareacceptable.GENFE2D
statesareacceptable.AFEquartic2D = statesareacceptable.GENFE2D

#
# calculation of state independent misfits
#

calc.sindepmisfits.AFElinear2D =  calc.sindepmisfits.GENFE2D
calc.sindepmisfits.AFEquadratic2D =  calc.sindepmisfits.GENFE2D
calc.sindepmisfits.AFEcubic2D =  calc.sindepmisfits.GENFE2D
calc.sindepmisfits.AFEquartic2D =  calc.sindepmisfits.GENFE2D

#
# calculation of state dependent misfits
#

calc.modmisfits.GENAFE2D = function(model)
{
  model$misfits[] = model$sindepmisfits[]
  isef = seq(along=model$sdepspatxflux)

  for(i in 1:model$Numelements)
  {
    nid1 = model$elements[i,"nid1"]
    nid2 = model$elements[i,"nid2"]
    nid3 = model$elements[i,"nid3"]
    A = model$elements[i,"area"]


    gradx = model$elements[i,"dg11"]*(model$states[nid2]-model$states[nid1])+
      model$elements[i,"dg12"]*(model$states[nid3]-model$states[nid1])

    grady = model$elements[i,"dg21"]*(model$states[nid2]-model$states[nid1])+
      model$elements[i,"dg22"]*(model$states[nid3]-model$states[nid1])

    fluxint1 = 0
    fluxint2 = 0
    fluxint3 = 0
    for(k in 1:model$Numgp)
    {
      ints = model$gamma[k,1]*model$states[nid1]+
        model$gamma[k,2]*model$states[nid2]+  model$gamma[k,3]*model$states[nid3]
      flux = model$weight[k]*model$systemflux(model$gpointelx[i,k],
                                                  model$gpointely[i,k],
                                                  ints,gradx,grady)

      fluxint1 = flux[1]*model$elements[i,"n1x"]+flux[2]*model$elements[i,"n1y"]
      fluxint2 = flux[1]*model$elements[i,"n2x"]+flux[2]*model$elements[i,"n2y"]
      fluxint3 = flux[1]*model$elements[i,"n3x"]+flux[2]*model$elements[i,"n3y"]

      for(j in isef)
      {
        value =  model$sdepspatxflux[[j]](model$gpointelx[i,k],
                                          model$gpointely[i,k],ints)
        fluxint1 = fluxint1+model$weight[k]*A*model$gamma[k,1]*value
        fluxint2 = fluxint2+model$weight[k]*A*model$gamma[k,2]*value
        fluxint3 = fluxint3+model$weight[k]*A*model$gamma[k,3]*value
      }
    }
    model$misfits[nid1] = model$misfits[nid1] + fluxint1
    model$misfits[nid2] = model$misfits[nid2] + fluxint2
    model$misfits[nid3] = model$misfits[nid3] + fluxint3
  }
  # now doing point external fluxes
  ipef = seq(along=model$sdeppntxflux)
  for(i in ipef)
  {
    pef = model$sdeppntxflux[[i]]
    el = model$elements[pef$indelem,]
    nid1 = el[["nid1"]]
    nid2 = el[["nid2"]]
    nid3 = el[["nid3"]]
    sint = pef$p1 * model$states[nid1]+
      pef$p2 * model$states[nid2]+
      pef$p3 * model$states[nid3]
    value = pef$func(sint)

    model$misfits[nid1] = model$misfits[nid1]+ pef$p1 * value
    model$misfits[nid2] = model$misfits[nid2]+ pef$p2 * value
    model$misfits[nid3] = model$misfits[nid3]+ pef$p3 * value
  }
  # now doing line external fluxes
  lpef = seq(along=model$linexflux)
  for(i in lpef)
  {
    lineflux = model$linexflux[[i]]
    sdep = lineflux$sdep
    for(j in seq(along=sdep[,1]))
    {
      svalue = sdep[j,"p1"] * model$states[sdep[j,"n1"]]+
        sdep[j,"p2"] * model$states[sdep[j,"n2"]]+
        sdep[j,"p3"] * model$states[sdep[j,"n3"]]
      nodesvalue = sdep[j,"p1"] * model$nodes[sdep[j,"n1"],]+
        sdep[j,"p2"] * model$nodes[sdep[j,"n2"],]+
        sdep[j,"p3"] * model$nodes[sdep[j,"n3"],]
      v = lineflux$values[[ sdep[j,"pointnum"] ]](
        nodesvalue[1],nodesvalue[2],svalue)*
        sdep[j,"weight"]
      model$misfits[sdep[j,"n1"]] =
        model$misfits[sdep[j,"n1"]]+v*sdep[j,"p1"]
      model$misfits[sdep[j,"n2"]] =
        model$misfits[sdep[j,"n2"]]+v*sdep[j,"p2"]
      model$misfits[sdep[j,"n3"]] =
        model$misfits[sdep[j,"n3"]]+v*sdep[j,"p3"]
    }
  }
  # now do boundary conditions
  for(i in 1:nrow(model$boundarypart))
  {
    segid = model$boundarypart[i,"domainsegid"]
    nodeid1 = model$boundarypart[i,"nid1"]
    nodeid2 = model$boundarypart[i,"nid2"]
    segBC = model$BC[[segid]]
    if(segBC$type=="fluxstate") {
      statemid = (model$states[nodeid1]+model$states[nodeid2])/2
      value = model$boundarypart[i,"domainlength"]*segBC$func(statemid)
      model$misfits[nodeid1] = model$misfits[nodeid1]+ 0.5 * value
      model$misfits[nodeid2] = model$misfits[nodeid2]+ 0.5 * value
    }
  }
}

calc.modmisfits.AFElinear2D =  calc.modmisfits.GENAFE2D
calc.modmisfits.AFEquadratic2D =  calc.modmisfits.GENAFE2D
calc.modmisfits.AFEcubic2D =  calc.modmisfits.GENAFE2D
calc.modmisfits.AFEquartic2D =  calc.modmisfits.GENAFE2D
#


# Calculate misfits
#

#' @export calc.misfits.AFLOW2D
calc.misfits.AFLOW2D = calc.misfits.FLOW2D

#' @export calc.misfits.AFElinear2D
calc.misfits.AFElinear2D = calc.misfits.GENFEFV2D

#' @export calc.misfits.AFEquadratic2D
calc.misfits.AFEquadratic2D = calc.misfits.GENFEFV2D

#' @export calc.misfits.AFEcubic2D
calc.misfits.AFEcubic2D = calc.misfits.GENFEFV2D

#' @export calc.misfits.AFEquartic2D
calc.misfits.AFEquartic2D = calc.misfits.GENFEFV2D


# RMSM
# Calculate RMSM

#' @export calc.RMSM.AFLOW2D
calc.RMSM.AFLOW2D = calc.RMSM.FLOW2D

#' @export calc.RMSM.AFElinear2D
calc.RMSM.AFElinear2D = calc.RMSM.GENFEFV2D

#' @export calc.RMSM.AFEquadratic2D
calc.RMSM.AFEquadratic2D = calc.RMSM.GENFEFV2D

#' @export calc.RMSM.AFEcubic2D
calc.RMSM.AFEcubic2D = calc.RMSM.GENFEFV2D

#' @export calc.RMSM.AFEquartic2D
calc.RMSM.AFEquartic2D = calc.RMSM.GENFEFV2D

# MAM
# Calculate MAM
#


#' @export calc.MAM.AFLOW2D
calc.MAM.AFLOW2D = calc.MAM.FLOW2D

#' @export calc.MAM.AFElinear2D
calc.MAM.AFElinear2D = calc.MAM.GENFEFV2D

#' @export calc.MAM.AFEquadratic2D
calc.MAM.AFEquadratic2D = calc.MAM.GENFEFV2D

#' @export calc.MAM.AFEcubic2D
calc.MAM.AFEcubic2D = calc.MAM.GENFEFV2D

#' @export calc.MAM.AFEquartic2D
calc.MAM.AFEquartic2D = calc.MAM.GENFEFV2D




#
# for Jacobian storage
#

make.Jacobstore.AFElinear2D = make.Jacobstore.GENFE2D
make.Jacobstore.AFEquadratic2D = make.Jacobstore.GENFE2D
make.Jacobstore.AFEcubic2D = make.Jacobstore.GENFE2D
make.Jacobstore.AFEquartic2D = make.Jacobstore.GENFE2D


calc.NumJacobian.GENAFE2D = function(model)
{
  model$Jacob@x[] = 0
  eps = calc.alleps(model)
  isef = seq(along=model$sdepspatxflux)

  for(ei in 1:model$Numelements)
  {
    nid1 = model$elements[ei,"nid1"]
    nid2 = model$elements[ei,"nid2"]
    nid3 = model$elements[ei,"nid3"]
    A = model$elements[ei,"area"]

    eps1 = eps[nid1]
    eps2 = eps[nid2]
    eps3 = eps[nid3]

    states1 = model$states[nid1]+c(0,eps1,0,0)
    states2 = model$states[nid2]+c(0,0,eps2,0)
    states3 = model$states[nid3]+c(0,0,0,eps3)

    gradx = model$elements[ei,"dg11"]*(states2-states1)+
      model$elements[ei,"dg12"]*(states3-states1)

    grady = model$elements[ei,"dg21"]*(states2-states1)+
      model$elements[ei,"dg22"]*(states3-states1)


    fluxint1 = rep(0,4)
    fluxint2 = rep(0,4)
    fluxint3 = rep(0,4)

    for(k in 1:model$Numgp)
    {
      ints = model$gamma[k,1]*states1+
        model$gamma[k,2]*states2+  model$gamma[k,3]*states3


      flux = model$weight[k]*model$systemflux(model$gpointelx[ei,k],
                                                  model$gpointely[ei,k],
                                                  ints,gradx,grady)

      fluxint1 = flux[1,]*model$elements[ei,"n1x"]+flux[2,]*model$elements[ei,"n1y"]
      fluxint2 = flux[1,]*model$elements[ei,"n2x"]+flux[2,]*model$elements[ei,"n2y"]
      fluxint3 = flux[1,]*model$elements[ei,"n3x"]+flux[2,]*model$elements[ei,"n3y"]


      for(j in isef)
      {
        value =   model$sdepspatxflux[[j]](model$gpointelx[ei,k],
                                           model$gpointely[ei,k],ints)

        fluxint1 = fluxint1+model$weight[k]*A*model$gamma[k,1]*value
        fluxint2 = fluxint2+model$weight[k]*A*model$gamma[k,2]*value
        fluxint3 = fluxint3+model$weight[k]*A*model$gamma[k,3]*value
      }

      model$Jacob@x[nid1] = model$Jacob@x[nid1]  + (fluxint1[2]-fluxint1[1])/eps1

      Jii =  model$elements[[ei,"Ji1i2"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii]  + (fluxint1[3]-fluxint1[1])/eps2

      Jii =  model$elements[[ei,"Ji1i3"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii]  + (fluxint1[4]-fluxint1[1])/eps3

      Jii =  model$elements[[ei,"Ji2i1"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii]  + (fluxint2[2]-fluxint2[1])/eps1

      model$Jacob@x[nid2] = model$Jacob@x[nid2] + (fluxint2[3]-fluxint2[1])/eps2

      Jii =  model$elements[[ei,"Ji2i3"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii]  + (fluxint2[4]-fluxint2[1])/eps3

      Jii =  model$elements[[ei,"Ji3i1"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii]  + (fluxint3[2]-fluxint3[1])/eps1

      Jii = model$elements[[ei,"Ji3i2"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii]  + (fluxint3[3]-fluxint3[1])/eps2

      model$Jacob@x[nid3] = model$Jacob@x[nid3] + (fluxint3[4]-fluxint3[1])/eps3
    }
  }
  #now doing point external fluxes
  ipef = seq(along=model$sdeppntxflux)
  for(i in ipef)
  {
    pef = model$sdeppntxflux[[i]]
    el = model$elements[pef$indelem,]
    nid1 = el[["nid1"]]
    nid2 = el[["nid2"]]
    nid3 = el[["nid3"]]

    eps1 = eps[nid1]
    eps2 = eps[nid2]
    eps3 = eps[nid3]

    states1 = model$states[nid1]+c(0,eps1,0,0)
    states2 = model$states[nid2]+c(0,0,eps2,0)
    states3 = model$states[nid3]+c(0,0,0,eps3)

    sint = pef$p1 * states1+
      pef$p2 * states2+
      pef$p3 * states3

    value = pef$func(sint)

    model$Jacob@x[nid1] = model$Jacob@x[nid1] + pef$p1*(value[2]-value[1])/eps1

    Jii  = el[["Ji1i2"]]
    model$Jacob@x[Jii] = model$Jacob@x[Jii] + pef$p1*(value[3]-value[1])/eps2

    Jii = el[["Ji1i3"]]
    model$Jacob@x[Jii] =  model$Jacob@x[Jii] +  pef$p1*(value[4]-value[1])/eps3

    Jii = el[["Ji2i1"]]
    model$Jacob@x[Jii] =  model$Jacob@x[Jii] + pef$p2*(value[2]-value[1])/eps1

    model$Jacob@x[nid2] = model$Jacob@x[nid2] +  pef$p2*(value[3]-value[1])/eps2

    Jii = el[["Ji2i3"]]
    model$Jacob@x[Jii] = model$Jacob@x[Jii] + pef$p2*(value[4]-value[1])/eps3

    Jii = el[["Ji3i1"]]
    model$Jacob@x[Jii] = model$Jacob@x[Jii] + pef$p3*(value[2]-value[1])/eps1

    Jii = el[["Ji3i2"]]
    model$Jacob@x[Jii] = model$Jacob@x[Jii] + pef$p3*(value[3]-value[1])/eps2

    model$Jacob@x[nid3] = model$Jacob@x[nid3] +  pef$p3*(value[4]-value[1])/eps3
  }
  # now doing line external fluxes
  lpef = seq(along=model$linexflux)
  for(i in lpef)
  {
    lineflux = model$linexflux[[i]]
    sdep = lineflux$sdep
    for(j in seq(along=sdep[,1]))
    {
      el   = model$elements[sdep[[j,"elnum"]],]
      nid1 = sdep[[j,"n1"]]
      nid2 = sdep[[j,"n2"]]
      nid3 = sdep[[j,"n3"]]

      eps1 = eps[nid1]
      eps2 = eps[nid2]
      eps3 = eps[nid3]

      states1 = model$states[nid1]+c(0,eps1,0,0)
      states2 = model$states[nid2]+c(0,0,eps2,0)
      states3 = model$states[nid3]+c(0,0,0,eps3)

      svalue = sdep[j,"p1"] * states1+
        sdep[j,"p2"] * states2+
        sdep[j,"p3"] * states3
      nodesvalue = sdep[j,"p1"] * model$nodes[nid1,]+
        sdep[j,"p2"] * model$nodes[nid2,]+
        sdep[j,"p3"] * model$nodes[nid3,]
      value = lineflux$values[[ sdep[j,"pointnum"] ]](
        nodesvalue[1],nodesvalue[2],svalue)*
        sdep[j,"weight"]

      model$Jacob@x[nid1] = model$Jacob@x[nid1] + sdep[j,"p1"]*(value[2]-value[1])/eps1

      Jii = el[["Ji1i2"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii] + sdep[j,"p1"]*(value[3]-value[1])/eps2

      Jii = el[["Ji1i3"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii] + sdep[j,"p1"]*(value[4]-value[1])/eps3

      Jii = el[["Ji2i3"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii] + sdep[j,"p2"]*(value[2]-value[1])/eps1

      model$Jacob@x[nid2] = model$Jacob@x[nid2] + sdep[j,"p2"]*(value[3]-value[1])/eps2

      Jii = el[["Ji2i3"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii] + sdep[j,"p2"]*(value[4]-value[1])/eps3

      Jii = el[["Ji3i1"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii] + sdep[j,"p3"]*(value[2]-value[1])/eps1

      Jii = el[["Ji3i2"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii] + sdep[j,"p3"]*(value[3]-value[1])/eps2

      model$Jacob@x[nid3] = model$Jacob@x[nid3] + sdep[j,"p3"]*(value[4]-value[1])/eps3
    }
  }
  # now do boundary conditions
  for(i in 1:nrow(model$boundarypart))
  {
    segid = model$boundarypart[i,"domainsegid"]
    nid1 = model$boundarypart[i,"nid1"]
    nid2 = model$boundarypart[i,"nid2"]
    eps1 = eps[nid1]
    eps2 = eps[nid2]
    states1 = model$states[nid1]+c(0,eps1,0)
    states2 = model$states[nid2]+c(0,0,eps2)

    segBC = model$BC[[segid]]
    if(segBC$type == "fluxstate")
    {
      statemid = (states1+states2)/2

      value = model$boundarypart[i,"domainlength"]*segBC$func(statemid)

      model$Jacob@x[nid1] = model$Jacob@x[nid1] + 0.5*(value[2]-value[1])/eps1

      Jii = model$boundarypart[[i,"Ji2i1"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii] + 0.5*(value[2]-value[1])/eps1

      Jii = model$boundarypart[[i,"Ji1i2"]]
      model$Jacob@x[Jii] = model$Jacob@x[Jii] + 0.5*(value[3]-value[1])/eps2

      model$Jacob@x[nid2] = model$Jacob@x[nid2] + 0.5*(value[3]-value[1])/eps2
    }
  }
}


calc.NumJacobian.AFElinear2D =  calc.NumJacobian.GENAFE2D
calc.NumJacobian.AFEquadratic2D =  calc.NumJacobian.GENAFE2D
calc.NumJacobian.AFEcubic2D =  calc.NumJacobian.GENAFE2D
calc.NumJacobian.AFEquartic2D =  calc.NumJacobian.GENAFE2D

#' @export solve.step.AFLOW2D
solve.step.AFLOW2D = solve.step.FLOW2D

#' @export solve.step.AFElinear2D
solve.step.AFElinear2D = solve.step.GENFEFV2D

#' @export solve.step.AFEquadratic2D
solve.step.AFEquadratic2D = solve.step.GENFEFV2D

#' @export solve.step.AFEcubic2D
solve.step.AFEcubic2D = solve.step.GENFEFV2D

#' @export solve.step.AFEquartic2D
solve.step.AFEquartic2D = solve.step.GENFEFV2D


#' @export solve.steps.AFLOW2D
solve.steps.AFLOW2D = solve.steps.FLOW2D

#' @export solve.steps.AFElinear2D
solve.steps.AFElinear2D = solve.steps.GENFEFV2D

#' @export solve.steps.AFEquadratic2D
solve.steps.AFEquadratic2D = solve.steps.GENFEFV2D

#' @export solve.steps.AFEcubic2D
solve.steps.AFEcubic2D = solve.steps.GENFEFV2D

#' @export solve.steps.AFEquartic2D
solve.steps.AFEquartic2D = solve.steps.GENFEFV2D

#' @export dataframe.nodes.AFLOW2D
dataframe.nodes.AFLOW2D = dataframe.nodes.FLOW2D

#' @export dataframe.nodes.AFElinear2D
dataframe.nodes.AFElinear2D = dataframe.nodes.GENFEFV2D

#' @export dataframe.nodes.AFEquadratic2D
dataframe.nodes.AFEquadratic2D = dataframe.nodes.GENFEFV2D

#' @export dataframe.nodes.AFEcubic2D
dataframe.nodes.AFEcubic2D = dataframe.nodes.GENFEFV2D

#' @export dataframe.nodes.AFEquartic2D
dataframe.nodes.AFEquartic2D = dataframe.nodes.GENFEFV2D


#' @export state.fun.AFLOW2D
state.fun.AFLOW2D = state.fun.FLOW2D

#' @export state.fun.AFElinear2D
state.fun.AFElinear2D = state.fun.GENFE2D

#' @export state.fun.AFEquadratic2D
state.fun.AFEquadratic2D = state.fun.GENFE2D

#' @export state.fun.AFEcubic2D
state.fun.AFEcubic2D = state.fun.GENFE2D

#' @export state.fun.AFEquartic2D
state.fun.AFEquartic2D = state.fun.GENFE2D


#' @export dataframe.states.AFLOW2D
dataframe.states.AFLOW2D = dataframe.states.FLOW2D

#' @export dataframe.states.AFElinear2D
dataframe.states.AFElinear2D = dataframe.states.GENFEFV2D

#' @export dataframe.states.AFEquadratic2D
dataframe.states.AFEquadratic2D = dataframe.states.GENFEFV2D

#' @export dataframe.states.AFEcubic2D
dataframe.states.AFEcubic2D = dataframe.states.GENFEFV2D

#' @export dataframe.states.AFEquartic2D
dataframe.states.AFEquartic2D = dataframe.states.GENFEFV2D


#' @export dataframe.internalfluxes.AFLOW2D
dataframe.internalfluxes.AFLOW2D = dataframe.internalfluxes.FLOW2D

dataframe.internalfluxes.GENAFE2D = function(model)
{
  result = data.frame()
  for(i in 1:model$Numelements)
  {
    nid1 = model$elements[i,"nid1"]
    nid2 = model$elements[i,"nid2"]
    nid3 = model$elements[i,"nid3"]
    A = model$elements[i,"area"]

    gradx = model$elements[i,"dg11"]*(model$states[nid2]-model$states[nid1])+
      model$elements[i,"dg12"]*(model$states[nid3]-model$states[nid1])

    grady = model$elements[i,"dg21"]*(model$states[nid2]-model$states[nid1])+
      model$elements[i,"dg22"]*(model$states[nid3]-model$states[nid1])

    fluxx = 0
    fluxy = 0


    fluxint1 = 0
    fluxint2 = 0
    fluxint3 = 0
    for(k in 1:model$Numgp)
    {
      ints = model$gamma[k,1]*model$states[nid1]+
        model$gamma[k,2]*model$states[nid2]+  model$gamma[k,3]*model$states[nid3]

      flux = model$weight[k]*model$systemflux(model$gpointelx[i,k],
                                                  model$gpointely[i,k],
                                                  ints,gradx,grady)
      fluxx = fluxx + flux[1]
      fluxy = fluxy + flux[2]
      fluxint1 = fluxint1+flux[1]*model$elements[i,"n1x"]+flux[2]*model$elements[i,"n1y"]
      fluxint2 = fluxint2+flux[1]*model$elements[i,"n2x"]+flux[2]*model$elements[i,"n2y"]
      fluxint3 = fluxint3+flux[1]*model$elements[i,"n3x"]+flux[2]*model$elements[i,"n3y"]
    }
    result = rbind(result,data.frame(node1=nid1,node2=nid2,node3=nid3,
                                     fluxto1=fluxint1,
                                     fluxto2=fluxint2,
                                     fluxto3=fluxint3,
                                     fluxx=fluxx,fluxy=fluxy))
  }
  rownames(result)=NULL
  return(result)
}

#' @export dataframe.internalfluxes.AFElinear2D
dataframe.internalfluxes.AFElinear2D = dataframe.internalfluxes.GENAFE2D

#' @export dataframe.internalfluxes.AFEquadratic2D
dataframe.internalfluxes.AFEquadratic2D = dataframe.internalfluxes.GENAFE2D

#' @export dataframe.internalfluxes.AFEcubic2D
dataframe.internalfluxes.AFEcubic2D = dataframe.internalfluxes.GENAFE2D

#' @export dataframe.internalfluxes.AFEquartic2D
dataframe.internalfluxes.AFEquartic2D = dataframe.internalfluxes.GENAFE2D


#' @export dataframe.externalfluxes.AFLOW2D
dataframe.externalfluxes.AFLOW2D = dataframe.externalfluxes.FLOW2D

#' @export dataframe.externalfluxes.AFElinear2D
dataframe.externalfluxes.AFElinear2D = dataframe.externalfluxes.GENFE2D

#' @export dataframe.externalfluxes.AFEquadratic2D
dataframe.externalfluxes.AFEquadratic2D = dataframe.externalfluxes.GENFE2D

#' @export dataframe.externalfluxes.AFEcubic2D
dataframe.externalfluxes.AFEcubic2D = dataframe.externalfluxes.GENFE2D

#' @export dataframe.externalfluxes.AFEquartic2D
dataframe.externalfluxes.AFEquartic2D = dataframe.externalfluxes.GENFE2D


#' @export dataframe.boundaries.AFLOW2D
dataframe.boundaries.AFLOW2D = dataframe.boundaries.FLOW2D

#' @export dataframe.boundaries.AFElinear2D
dataframe.boundaries.AFElinear2D = dataframe.boundaries.GENFE2D

#' @export dataframe.boundaries.AFEquadratic2D
dataframe.boundaries.AFEquadratic2D = dataframe.boundaries.GENFE2D

#' @export dataframe.boundaries.AFEcubic2D
dataframe.boundaries.AFEcubic2D = dataframe.boundaries.GENFE2D

#' @export dataframe.boundaries.AFEquartic2D
dataframe.boundaries.AFEquartic2D = dataframe.boundaries.GENFE2D


#' @export dataframe.balance.AFLOW2D
dataframe.balance.AFLOW2D = dataframe.balance.FLOW2D

#' @export dataframe.balance.AFElinear2D
dataframe.balance.AFElinear2D = dataframe.balance.GENFE2D

#' @export dataframe.balance.AFEquadratic2D
dataframe.balance.AFEquadratic2D = dataframe.balance.GENFE2D

#' @export dataframe.balance.AFEcubic2D
dataframe.balance.AFEcubic2D = dataframe.balance.GENFE2D

#' @export dataframe.balance.AFEquartic2D
dataframe.balance.AFEquartic2D = dataframe.balance.GENFE2D

#' @export summary.AFLOW2D
summary.AFLOW2D = summary.FLOW2D

#' @export summary.AFElinear2D
summary.AFElinear2D = summary.GENFEFV2D

#' @export summary.AFEquadratic2D
summary.AFEquadratic2D = summary.GENFEFV2D

#' @export summary.AFEcubic2D
summary.AFEcubic2D = summary.GENFEFV2D

#' @export summary.AFEquartic2D
summary.AFEquartic2D = summary.GENFEFV2D

#' @export plot.AFLOW2D
plot.AFLOW2D = plot.FLOW2D

#' @export plot.AFElinear2D
plot.AFElinear2D = plot.GENFE2D

#' @export plot.AFEquadratic2D
plot.AFEquadratic2D = plot.GENFE2D

#' @export plot.AFEcubic2D
plot.AFEcubic2D = plot.GENFE2D

#' @export plot.AFEquartic2D
plot.AFEquartic2D = plot.GENFE2D
