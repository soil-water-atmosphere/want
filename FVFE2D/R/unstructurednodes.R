#' A distance to point function constructor
#' @description A constructor of a function that gives one value 
#' at a specified point, another value far from the given point and a smooth 
#' transition in between. Such a function can be usefull for instance in 
#' \code{add.spatialflux} and \code{set.discretisation}.
#' @param point a vector giving the coordinates of the point
#' @param width the distance from the point from which the far value 
#' will be returned 
#' @param atvalue the value returned at the point
#' @param farvalue the value returned further than width from the point
#' @param vectorarg (default \code{TRUE}): if TRUE the argument of the returned 
#' function should be a vector (\code{c(x,y)}), if FALSE the returned function
#' needs two arguments (\code{x} and \code{y})
#' @return a function specified above
#' @export func.dist.to.point
#' @examples
#' point=c(4,6)
#' foo1 = func.dist.to.point(point,width=4,atvalue=4,farvalue=1)
#' foo1(c(5,5))
#' foo2 = func.dist.to.point(point,width=4,atvalue=4,farvalue=1,vectorarg=FALSE)
#' foo2(5,5)
#' xforplot = seq(0,10,length=60)
#' yforplot = seq(0,10,length=60)
#' zforplot = outer(xforplot,yforplot,foo2)
#' surface3d(xforplot,yforplot,zforplot,col="red",alpha=0.8)

func.dist.to.point = function(point,width,atvalue,farvalue,vectorarg=TRUE)
{
  dvalue = atvalue-farvalue
  f = function(q)
  {
    q = as.vector(q )
    dist = sqrt((point[1]-q[1])^2+(point[2]-q[2])^2)
    if(dist>width)
    {
      return(farvalue)
    } else {
      x = dist/width
      return(farvalue+dvalue*(1-3*x^2+2*x^3))
    }
  }
  if(!vectorarg)
  {
    f2 = function(x,y){return(f(c(x,y)))}
    return(Vectorize(f2))
  }
  return(f)
}

#' A distance to a line function constructor
#' @description A constructor of a function that gives one value 
#' on a specified line, another value far from the given line and a smooth 
#' transition in between. Such a function can be usefull for instance in 
#' \code{add.spatialflux} and \code{set.discretisation}.
#' @param line a matrix of two columns giving the coordinates of the line
#' @param width the distance from the line from which the far value 
#' will be returned 
#' @param atvalue the value returned at the line
#' @param farvalue the value returned further than width from the line
#' @param vectorarg (default \code{TRUE}): if TRUE the argument of the returned 
#' function should be a vector (\code{c(x,y)}), if FALSE the returned function
#' needs two arguments (\code{x} and \code{y})
#' @return a function specified above
#' @export func.dist.to.line
#' @examples
#' line = matrix(c(2,2,3,5,8,6),ncol=2,byrow=TRUE)
#' foo1 = func.dist.to.line(line,width=4,atvalue=4,farvalue=1)
#' foo1(c(2,3))
#' foo2= func.dist.to.line(line,width=4,atvalue=4,farvalue=1,vectorarg=FALSE)
#' foo2(2,3)
#' xforplot = seq(0,10,length=60)
#' yforplot = seq(0,10,length=60)
#' zforplot = outer(xforplot,yforplot,foo2)
#' surface3d(xforplot,yforplot,zforplot,col="red",alpha=0.8)

func.dist.to.line = function(line,width,atvalue,farvalue,vectorarg=TRUE)
{
  dvalue = atvalue-farvalue
  f = function(q)
  {
    q = as.vector(q)
    
    dist = +Inf
    for(i in 2:nrow(line))
    {
      dist=min(dist,GEO$dist.segment.point(line[(i-1):i,],q))
    }
    dist=sqrt(dist)
    if(dist>width)
    {
      return(farvalue)
    } else {
      x = dist/width
      return(farvalue+dvalue*(1-3*x^2+2*x^3))
    }    
  }
  if(!vectorarg)
  {
    f2 = function(x,y){return(f(c(x,y)))}
    return(Vectorize(f2))
  }
  return(f)
}



#' A distance to a polygon function constructor
#' @description A constructor of a function that gives one value 
#' inside a given polygon, another value far from the given polygon and a smooth 
#' transition in between. Such a function can be usefull for instance in 
#' \code{add.spatialflux} and \code{set.discretisation}.
#' @param poly a matrix of two columns giving the coordinates of the corners
#' of the polygon
#' @param width the distance from the polygon from which the far value 
#' will be returned 
#' @param invalue the value returned inside the polygon
#' @param farvalue the value returned further than width from the polygon
#' @param vectorarg (default \code{TRUE}): if TRUE the argument of the returned 
#' function should be a vector (\code{c(x,y)}), if FALSE the returned function
#' needs two arguments (\code{x} and \code{y})
#' @return a function specified above
#' @export func.dist.to.poly
#' @examples
#' poly = matrix(c(2.5,2.5,3,5,8,6),ncol=2,byrow=TRUE)
#' foo1 = func.dist.to.poly(poly,width=4,invalue=4,farvalue=1)
#' foo1(c(2,3))
#' foo2 = func.dist.to.poly(poly,width=4,invalue=4,farvalue=1,vectorarg=FALSE)
#' foo2(2,3)
#' xforplot = seq(0,10,length=60)
#' yforplot = seq(0,10,length=60)
#' zforplot = outer(xforplot,yforplot,foo2)
#' surface3d(xforplot,yforplot,zforplot,col="red",alpha=0.8)

func.dist.to.poly = function(poly,width,invalue,farvalue,vectorarg=TRUE)
{
  dvalue = invalue-farvalue
  f = function(q)
  {
    q = as.vector(q)
    if(GEO$inside.poly(poly,q))
    {
      return(invalue)
    }
    dist = sqrt(GEO$dist.poly.point(poly,q))
    if(dist>width)
    {
      return(farvalue)
    } else {
      x = dist/width
      return(farvalue+dvalue*(1-3*x^2+2*x^3))
    }    
  }
  if(!vectorarg)
  {
    f2 = function(x,y){return(f(c(x,y)))}
    return(Vectorize(f2))
  }
  return(f) 
}

# generate random nodes
simulleftright = function(lmin,lmax)
{
  a = sample(c(-1,1),1)
  r = runif(1,min=lmin,max=lmax)
  return(r*a)
}

simround = function(rmin,rmax)
{
  a = runif(1,min=0,max=2 *pi)
  r = runif(1,min=rmin,max=rmax)
  x = r * cos(a)
  y = r * sin(a)
  return(c(x,y))
}
# generate random nodes

randnewnodesonsegment = function(xy,df)
{
  il = 1
  dl = xy[2,il]-xy[1,il]
  if(abs(dl)<1e-10)
  {
    il = 2
    dl = xy[2,il]-xy[1,il]
  }
  calc.lambda = function(p)
  {
    lambda = (p[il]-xy[1,il])/dl
    return(lambda)
  }
  
  maxtries = 10
  
  nodesp  = xy
  nodesl = c(0,1)
  L = sqrt((xy[2,1]-xy[1,1])^2+(xy[2,2]-xy[1,2])^2)
  deltaline = (xy[2,]-xy[1,])/L
  
  numtries = c()
  # starting up
  numtries = c(2,2)
  activenodes = c(1,2)
  N = 2
  while(length(activenodes)>0)
  {
    i = sample(activenodes,size=1)
    minr = df(nodesp[i,])
    maxr = 1.2*minr
    
    newr = simulleftright(minr,maxr)
    
    newp = nodesp[i,] +newr*deltaline
    lambda = calc.lambda(newp)
    
    if((lambda>0)&(lambda<1))
    {
      if((min((nodesp[,1]-newp[1])^2+(nodesp[,2]-newp[2])^2)>minr^2))
      {
        nodesp = rbind(nodesp,newp)
        nodesl = c(nodesl,lambda)
        N = N+1
        activenodes=c(activenodes,N)
        numtries[N] = 1
      } else {
        numtries[i]  = numtries[i]+1
        if(numtries[i]>maxtries)
        {
          activenodes = setdiff(activenodes,i) 
        }
      }
    }
  }
  
  nodesl=sort(nodesl)
  nodesl
  lnodesl = length(nodesl)-1
  if(lnodesl>2)
  {
    nodesl[2:lnodesl] = (nodesl[1:(lnodesl-1)]+nodesl[3:(lnodesl+1)])/2
  }
  return(matrix(c(xy[1,1]+nodesl*(xy[2,1]-xy[1,1]),xy[1,2]+nodesl*(xy[2,2]-xy[1,2])),ncol=2))
}

random.border.nodes = function(
  domain,sdf,
  startnodes=NULL,
  graphverbose=TRUE,
  sleeptime=0)
{
  allnodes = domain
  if(graphverbose)
  {
    plot(domain,main=paste("num nodes=",nrow(domain)),
         xaxt="n",yaxt="n",xlab="x",ylab="y")
    polygon(domain,col=rgb(0.6,0.6,0.6,0.2))
    for(k in 1:nrow(domain))
    {
      points(domain[k,1],domain[k,2],col=rgb(1,0,0,0.8),pch=10,cex=0.75)
      draw.circle(domain[k,1],domain[k,2],sdf(domain[k,]),
                  border=NA,col=rgb(0,0,1,0.11))        
    }
    Sys.sleep(sleeptime)
  }
  newnodes = randnewnodesonsegment(domain[c(nrow(domain),1),],sdf)
  if(nrow(newnodes>2))
  {
    allnodes = rbind(domain,newnodes[2:(nrow(newnodes)-1),])
    if(graphverbose)
    {
      plot(domain,main=paste("num nodes=",nrow(allnodes)),
           xaxt="n",yaxt="n",xlab="x",ylab="y")
      polygon(domain,col=rgb(0.6,0.6,0.6,0.2))
      for(k in 1:nrow(allnodes))
      {
        points(allnodes[k,1],allnodes[k,2],col=rgb(1,0,0,0.8),pch=10,cex=0.75)
        draw.circle(allnodes[k,1],allnodes[k,2],sdf(allnodes[k,]),
                    border=NA,col=rgb(0,0,1,0.11))        
      }
      Sys.sleep(sleeptime)
    }
  }
  
  for(i in 2:nrow(domain))
  {
    newnodes = randnewnodesonsegment(domain[c(i-1,i),],sdf)
    if(nrow(newnodes>2))
    {
      allnodes = rbind(allnodes,newnodes[2:(nrow(newnodes)-1),])
      if(graphverbose)
      {
        plot(domain,main=paste("num nodes=",nrow(allnodes)),
             xaxt="n",yaxt="n",xlab="x",ylab="y")
        polygon(domain,col=rgb(0.6,0.6,0.6,0.2))
        for(k in 1:nrow(allnodes))
        {
          points(allnodes[k,1],allnodes[k,2],col=rgb(1,0,0,0.8),pch=10,cex=0.75)
          draw.circle(allnodes[k,1],allnodes[k,2],sdf(allnodes[k,]),
                      border=NA,col=rgb(0,0,1,0.11))        
        }
      }
    }
  } 
  return(allnodes)
}

random.inside.nodes = function(domain,sdf,maxfails,
                               startnodes= NULL,
                               graphverbose=TRUE,
                               sleeptime=0)
{
  
  if(is.null(startnodes)) startnodes = matrix(numeric(0),nrow=0,ncol=2)
  
  allnodes = startnodes
  maxtries = maxfails
  
  # starting up
  
  Nboundary = nrow(allnodes)
  N = Nboundary
  activenodes = 1:N
  numtries = rep(0,N)
  
  while(length(activenodes)>0)
  {
    i = sample(activenodes,size=1)
    minr = sdf(allnodes[i,])
    maxr = 1.6*minr#1.2*minr
    newdxdy = simround(minr,maxr)
    newp = allnodes[i,]+newdxdy
    if(GEO$inside.poly(domain,newp))
    {
      if((min((allnodes[,1]-newp[1])^2+(allnodes[,2]-newp[2])^2)>minr^2))
      {
        allnodes = rbind(allnodes,newp)
        N = N+1
        activenodes=c(activenodes,N)
        numtries[N] = 1
        if(graphverbose)
        {
          plot(domain,main=paste("num nodes=",nrow(allnodes)),
               xaxt="n",yaxt="n",xlab="x",ylab="y")
          polygon(domain,col=rgb(0.6,0.6,0.6,0.2))
          for(k in 1:nrow(allnodes))
          {
            points(allnodes[k,1],allnodes[k,2],col=rgb(1,0,0,0.8),pch=10,cex=0.75)
            draw.circle(allnodes[k,1],allnodes[k,2],sdf(allnodes[k,]),
                        border=NA,col=rgb(0,0,1,0.11))        
          }
          Sys.sleep(sleeptime)
        }
      } else {
        numtries[i]  = numtries[i]+1
        if(numtries[i]>maxtries)
        {
          activenodes = setdiff(activenodes,i) 
        }
      }
    }
  }
  return(allnodes)
}


randomnodes = function(model,mindist,maxfails=30,
                       startnodes= NULL, 
                       graphverbose = FALSE,
                       sleeptime=0)
{
  if(is.function(mindist))
  {
    sdf =  function(p){return(mindist(model$dounscale(matrix(p,nrow=1)))/model$scale)}
  } else
  {
    sdf = function(p){return(mindist/model$scale)}
  }
  
  bnodes = random.border.nodes(model$sydomain,sdf,
                               startnodes=model$sydomain,
                               graphverbose,sleeptime)
  
  if(!is.null(startnodes))
  {
    bnodes = rbind(bnodes,model$doscale(startnodes))
  }
  allnodes = random.inside.nodes(model$sydomain,sdf,maxfails,
                                 bnodes,graphverbose,sleeptime)
  return(model$dounscale(allnodes))
}

# geo 
geonewnodesonsegment = function(xy,distfunc)
{
  L = sqrt((xy[2,1]-xy[1,1])^2+(xy[2,2]-xy[1,2])^2) 
  Ndis = 200
  lseq = seq(0,1,length=Ndis)
  dseq= sapply(lseq, function(l){ return(distfunc(l*xy[1,]+(1-l)*xy[2,]))})
  Dseq = cumsum(1/dseq)/Ndis
  Dseq = Dseq-Dseq[1]
  DseqL = Dseq[Ndis]
  M = L*round(DseqL)+1
  lint = approx(Dseq,lseq,seq(0,DseqL,length=M))$y
  #p= sapply(lseq, function(l){ return(l*xy[,1]+(1-l)*xy[,2])}) 
  result = matrix(0,nrow=0,ncol=2)
  if(length(lint)>2)
    for(i in 2:(length(lint)-1))
    {
      f = lint[i]
      p = f*xy[1,]+(1-f)*xy[2,]
      result = rbind(result,p)
    }
  return(result[])
}

geomakenodesonborder = function(d,rf)
{
  allnodes = d
  
  newnodes = geonewnodesonsegment(d[c(nrow(d),1),],rf)
  allnodes = rbind(d,newnodes)
  for(i in 2:nrow(d))
  {
    newnodes = geonewnodesonsegment(d[c(i-1,i),],rf)
    allnodes = rbind(allnodes,newnodes)
  } 
  return(allnodes)
}


geomnodesindomain = function(d,rf,bordernodes,graphverbose,ytoxratio,sleeptime)
{
  allnodes = bordernodes
  
  maxtries = 16
  
  Nboundary = nrow(allnodes)
  N = Nboundary
  activenodes = 1:N
  numtries = rep(0,N)
  ii = 1
  while(length(activenodes)>0)
  {
    if(ii>length(activenodes)) ii = 1
    i = activenodes[ii]
    
    optr = rf(allnodes[i,])
    a = 2*pi*numtries[i]/(maxtries-1)
    newdxdy = c(optr*cos(a),optr*sin(a))
    newp = allnodes[i,]+newdxdy
    
    if(GEO$inside.poly(d,newp)&
         (min((allnodes[,1]-newp[1])^2+(allnodes[,2]-newp[2])^2)>0.9*optr^2))
    {
      
      allnodes = rbind(allnodes,newp)
      N = N+1
      activenodes=c(activenodes,N)
      numtries[N] = 1
      if(graphverbose)
      {
        if(N%%20==0)
        {
          plot(d,
               main=paste("num nodes =",nrow(allnodes)),
               xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="x",ylab="y")
          polygon(d,col=rgb(0.6,0.6,0.6,0.2))
          points(allnodes,col=rgb(1,0,0,0.8),pch=10,cex=0.75)
          for(i in 1:nrow(allnodes))
          {
            draw.circle(allnodes[i,1],allnodes[i,2],
                        rf(allnodes[i,,drop=FALSE]),
                        border=NA,col=rgb(0,0,1,0.11))
          }      
        }
        Sys.sleep(sleeptime)
      }
      ii=ii+1
    } else if(numtries[i] < maxtries)
    {
      #print("case B")
      numtries[i]  = numtries[i]+1
    } else {
      #print("case C")
      activenodes = setdiff(activenodes,i) 
    }
  }
  return(allnodes)
}

geomnodes = function(model,dist,
                     maxiter=50,
                     movefactor,
                     removefactor,
                     createfactor,
                     stopcrit=0.01,
                     startnodes = NULL,
                     graphverbose = FALSE,
                     sleeptime=0)
{ 
  if(!is.function(dist))
  {
    sdf = function(p){return(dist/model$scale)}
  } else {
    sdf = function(p)
    {
      return(dist(model$dounscale(p))/model$scale)
    }
  }
  
  if(!is.function(movefactor))
  {
    mvfun = function(iter){return(movefactor)}
  } else {
    mvfun = movefactor
  }
  
  if(!is.function(removefactor))
  {
    rmfun = function(iter){return(removefactor)}
  } else {
    rmfun = removefactor
  }
  
  if(!is.function(createfactor))
  {
    crfun = function(iter){return(createfactor)}
  } else {
    crfun = createfactor
  }
  
  
  ponboundary = c()
  
  
  allp = geomakenodesonborder(model$sydomain,sdf)    
  
  if(!is.null(startnodes))
  {
    allp = rbind(allp,model$doscale(startnodes))
  }
  Nboundary=nrow(allp)
  allp = geomnodesindomain(model$sydomain,sdf,allp,graphverbose,model$ytoxratio,sleeptime)
  
  if(!is.null(startnodes))
  {
    allp  = rbind(allp,model$doscale(startnodes))
  }
  
  if(graphverbose)
  {
    plot(model$sydomain,main=paste("num nodes =",nrow(allp)),
         xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="x",ylab="y")
    polygon(model$sydomain,col=rgb(0.6,0.6,0.6,0.2))
    points(allp,col=rgb(1,0,0,0.8),pch=10,cex=0.75)
    for(i in 1:nrow(allp))
    {
      draw.circle(allp[i,1],allp[i,2],
                  sdf(allp[i,,drop=FALSE]),
                  border=NA,col=rgb(0,0,1,0.11))
    }      
  }
  
  d = deldir(allp[,1],allp[,2])
  tri = triang.list(d)
  
  nodes = allp
  
  crit=+Inf
  
  for(iter in 1:maxiter)
  {
    curmovefactor = mvfun(iter)
    curremovefactor = rmfun(iter)
    curcreatefactor = crfun(iter)
    if(graphverbose)
    {
      plot(model$sydomain,
           main=paste("iter=",iter,"; crit =",formatC(crit,format="f",digits=6)),
           xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="x",ylab="y")
      mtext(paste("m=",formatC(curmovefactor,format="f",digits=4),
                  " r=",formatC(curremovefactor,format="f",digits=4),
                  " c=",formatC(curcreatefactor,format="f",digits=4),sep=""))
      polygon(model$sydomain,col=rgb(0.6,0.6,0.6,0.2))
      points(nodes,col=rgb(1,0,0,0.8),pch=10,cex=0.75)
      
      for(i in 1:nrow(nodes))
      {
        draw.circle(nodes[i,1],nodes[i,2],
                    sdf(nodes[i,,drop=FALSE]),
                    border=NA,col=rgb(0,0,1,0.11))
      }   
      Sys.sleep(sleeptime)
    }
    d = deldir(nodes[,1],nodes[,2])
    moveDE= matrix(0,nrow=nrow(nodes),ncol=3)
    toremove = rep(FALSE,nrow(nodes))
    tri = triang.list(d)
    newnodes = matrix(0,nrow=0,ncol=2)
    for(i in 1:length(tri))
    {
      tt = tri[[i]]
      midp = matrix(c(mean(tt[,"x"]),mean(tt[,"y"])),ncol=2)
      if(GEO$inside.poly(model$sydomain,midp))
      {  
        v12 = c(tt[1,"x"]-tt[2,"x"],tt[1,"y"]-tt[2,"y"])
        v23 = c(tt[2,"x"]-tt[3,"x"],tt[2,"y"]-tt[3,"y"])
        v31 = c(tt[3,"x"]-tt[1,"x"],tt[3,"y"]-tt[1,"y"])
        l12s= v12[1]^2+v12[2]^2
        l23s= v23[1]^2+v23[2]^2
        l31s= v31[1]^2+v31[2]^2
        los = sdf(midp)^2
        D12 = v12 * (l12s-los)
        D23 = v23 * (l23s-los)
        D31 = v31 * (l31s-los)
        E12 = (l12s-los)+2*l12s
        E23 = (l23s-los)+2*l23s
        E31 = (l31s-los)+2*l31s
        moveDE[tt[1,"ptNum"],] = moveDE[tt[1,"ptNum"],]+c(-D12+D31,E12+E31)
        moveDE[tt[2,"ptNum"],] = moveDE[tt[2,"ptNum"],]+c(-D23+D12,E23+E12)
        moveDE[tt[3,"ptNum"],] = moveDE[tt[3,"ptNum"],]+c(-D31+D23,E31+E23)               
        if(min(l12s,l23s,l31s)<curremovefactor*los)
        {
          k = max(tt[,"ptNum"])
          if(k>Nboundary)
          {
            toremove[k]=TRUE
          }
        }
        if(max(l12s,l23s,l31s)>curcreatefactor*los) 
        {
          A = GEO$area(tt[1,c("x","y")],tt[2,c("x","y")],tt[3,c("x","y")])
          semiper = (sqrt(l12s)+sqrt(l23s)+sqrt(l31s))/2
          Rincircle  = A/semiper
          if(Rincircle>curcreatefactor*los*0.75)
          {
            newnodes = rbind(newnodes,midp)
          }
        }        
      }
    }
    moveDE[1:Nboundary,1:2]=0
    moveDE[1:Nboundary,3]=1
    move =  curmovefactor*moveDE[,c(1,2)]/moveDE[,3]
    move[is.nan(move)]=0
    movednodes = nodes + move 
    movesizes = sqrt(move[,1]^2+move[,2]^2)
    if(graphverbose)
    {
      I = (movesizes>0.005)
      arrows(nodes[I,1],nodes[I,2],movednodes[I,1],movednodes[I,2],length=0.1)
    }
    crit=max(movesizes)+(nrow(newnodes)+sum(toremove==TRUE))/(1+length(movednodes))   
    #    print(paste("iter=",iter,"; crit=",crit))
    if(crit<stopcrit) break;
    if(graphverbose)
    {
      points(nodes[toremove,],col="yellow",pch=10,cex=2)
      points(newnodes,col="green",pch=10,cex=2)
    }
    nodes=nodes[1:Nboundary,]
    if(Nboundary<nrow(movednodes))
    {
      for(i in (Nboundary+1):nrow(movednodes))
      {
        if(GEO$inside.poly(model$sydomain,movednodes[i,])&(toremove[i]==FALSE))
        {
          nodes = rbind(nodes,movednodes[i,])
        }
      }
    }
    nodes=rbind(nodes,newnodes)
    
    if(graphverbose)
    {
      Sys.sleep(0.4)
    }
  }
  if(graphverbose)
  {
    plot(model$sydomain,
         main=paste("iter=",iter,"; crit =",formatC(crit,format="f",digits=6)),
         xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="x",ylab="y")
    polygon(model$sydomain,col=rgb(0.6,0.6,0.6,0.2))
    points(nodes,col=rgb(1,0,0,0.8),pch=10,cex=0.75)
    
    for(i in 1:nrow(nodes))
    {
      draw.circle(nodes[i,1],nodes[i,2],
                  sdf(nodes[i,,drop=FALSE]),
                  border=NA,col=rgb(0,0,1,0.11))
    }     
    Sys.sleep(sleeptime)
  }
  rownames(nodes)=NULL
  # remove nodes to close 
  selnodes = matrix(nodes[1,],nrow=1,ncol=2)
  for(k in 2:nrow(nodes))
  {
    for(j in 1:nrow(selnodes))
    {
      djk = (selnodes[j,1]-nodes[k,1])^2+(selnodes[j,2]-nodes[k,2])^2
      if(djk < 1e-8) break
    }
    if(djk > 1e-8)
    {
      selnodes = rbind(selnodes,nodes[k,])
    }
  }
  return(model$dounscale(selnodes))
}



