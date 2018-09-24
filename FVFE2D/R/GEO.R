
GEO = new.env()

# very basic implementations of some geometrical stuff

# we need something to be called "small"

GEO$eps = 1e-12 #1e-06

# a point is a vector with two entries
# p's in the argument that follows are points


GEO$ccw  = function(p1,p2,p3)
{
  return((p3[2]-p1[2])*(p2[1]-p1[1])-(p2[2]-p1[2])*(p3[1]-p1[1]))
}

GEO$area = function(p1,p2,p3)
{
  return(abs((p2[1]*p1[2]-p1[1]*p2[2])+(p3[1]*p2[2]-p2[1]*p3[2])+(p1[1]*p3[2]-p3[1]*p1[2]))/2)
}

# a segment is a two by two matrix, rows are endpoints
GEO$segments.intersect = function(seg1,seg2)
{
  test1 = GEO$ccw(seg1[1,], seg1[2,], seg2[1,])* GEO$ccw(seg1[1,], seg1[2,],seg2[2,])
  if(test1>0) return(FALSE)
  test2 = GEO$ccw(seg2[1,],seg2[2,], seg1[1,])* GEO$ccw(seg2[1,], seg2[2,],seg1[2,])
  return (test2 <= 0)
}

# find frac of segment closest to point

GEO$frac.segment.point = function(s,p)
{
  l = ((s[2,1]-s[1,1])*(p[1]-s[1,1])+(s[2,2]-s[1,2])*(p[2]-s[1,2]))/
    ((s[2,1]-s[1,1])^2+(s[2,2]-s[1,2])^2)
  if(l > 1)
  {
    l = 1
  } else if(l < 0)
  {
    l=0
  }
  return(l)
}

# distance between segment and point

GEO$dist.segment.point = function(s,p)
{
  l  = GEO$frac.segment.point(s,p)
  return((s[1,1]-p[1]+l*(s[2,1]-s[1,1]))^2+(s[1,2]-p[2]+l*(s[2,2]-s[1,2]))^2)
}

# find closest segment of poly to point

GEO$dist.poly.point = function(poly,p)
{
  N = nrow(poly)
  mindist = c(GEO$dist.segment.point(poly[c(N,1),],p))
  for(i in 2:N)
  {
    mindist = min(mindist,GEO$dist.segment.point(poly[c(i-1,i),],p))
  }
  return(mindist)
}

# returns a clipped (part) of the segment inside the polygon
# return value is a matrix of two rows x y segnum
# segnum = -1 if point is not on segment of domain
GEO$clip.segment.in.poly  = function(poly,seg)
{
  pa  = matrix(seg[1,],ncol=2)
  pb  = matrix(seg[2,],ncol=2)
  if(GEO$inside.poly(poly,pa))
  {
    pai = cbind(pa,-1)
    if(GEO$inside.poly(poly,pb))
    {
      pbi = cbind(pb,-1)
      result = rbind(pai,pbi)
      colnames(result)=c("x","y","segnum")
    } else {
      ps = GEO$intersection.poly.segment(poly,seg)
      result = rbind(pai,ps)
      colnames(result)=c("x","y","segnum")
    }
  } else { #pa not in
    if(GEO$inside.poly(poly,pb)) {
      ps = GEO$intersection.poly.segment(poly,seg)
      pbi = cbind(pb,-1)
      result = rbind(ps,pbi)
    } else { #both are out
      result= GEO$intersection.poly.segment(poly,seg)
    }
  }
  return(result)
}

# area of polygon

GEO$area.of.poly = function(poly)
{
  N = nrow(poly)
  area = poly[N,1]*poly[1,2] - poly[1,1]*poly[N,2]
  for(i in 1:(N-1))
  {
    area = area + poly[i,1]*poly[i+1,2] - poly[i+1,1]*poly[i,2]
  }
  return(abs(area)/2)
}

# find closest segment of poly to point

GEO$isegnum = function(poly,p)
{
  N = nrow(poly)
  alldist=c()
  for(i in 2:N)
  {
    alldist = c(alldist,GEO$dist.segment.point(poly[c(i-1,i),],p))
  }
  alldist = c(alldist,GEO$dist.segment.point(poly[c(N,1),],p))
  return(which.min(alldist))
}

# calc fraction left of closest point on segment to p

GEO$frac.with.polyisegnum = function(poly, isegnum,p)
{
  if(isegnum < nrow(poly))
  {
    frac= GEO$frac.segment.point(poly[c(isegnum,isegnum+1),],p)
  } else
  {
    frac=GEO$frac.segment.point(poly[c(nrow(poly),1),],p)
  }
  return(frac)
}

GEO$inside.poly = function(domain,p)
{
  result = FALSE
  j = nrow(domain)
  for(i in 1:nrow(domain))
  {
    if ( ((domain[i,2] > p[2]) != (domain[j,2] > p[2])) &&
         (p[1] < (domain[j,1]-domain[i,1]) * (p[2]-domain[i,2]) /
          (domain[j,2]-domain[i,2]) + domain[i,1]))
    {
      result = ! result
    }
    j=i
  }
  return(result)
}




# gives intersectionpoint or NULL if no intersection

GEO$intersection.segment.segment = function(seg1,seg2)
{
  Det = (seg1[1,1]-seg1[2,1])*(seg2[2,2]-seg2[1,2])-(seg1[1,2]-seg1[2,2])*(seg2[2,1]-seg2[1,1])
  if(Det==0) return(NULL)
  l = ((seg2[2,2]-seg2[1,2])*(seg2[2,1]-seg1[2,1])+(seg2[1,1]-seg2[2,1])*(seg2[2,2]-seg1[2,2]))/Det
  if((l<0)|(l>1)) return(NULL)
  m = ((seg1[2,2]-seg1[1,2])*(seg2[2,1]-seg1[2,1])+(seg1[1,1]-seg1[2,1])*(seg2[2,2]-seg1[2,2]))/Det
  if((m<0)|(m>1)) return(NULL)
  x = (l*seg1[1,1]+(1-l)*seg1[2,1]+m*seg2[1,1]+(1-m)*seg2[2,1])/2
  y = (l*seg1[1,2]+(1-l)*seg1[2,2]+m*seg2[1,2]+(1-m)*seg2[2,2])/2
  return(c(x,y))
}

# returns a list, first intersection vector or NULL  second number of segment or -1

GEO$intersection.poly.segment = function(poly,seg)
{
  allps = NULL
  N = dim(poly)[1]
  if(N>1)
  {
    for(i in 2:N)
    {
      p = GEO$intersection.segment.segment(poly[(i-1):i,],seg)
      if(!is.null(p))
      {
        allps = rbind(allps,c(p[1],p[2],i-1))
      }
    }
    p = GEO$intersection.segment.segment(rbind(poly[N,],poly[1,]),seg)
    if(!is.null(p))
    {
      allps = rbind(allps,c(p[1],p[2],N))
    }
    if(length(allps)>0)
    {
      colnames(allps) = c("x","y","segnum")
    }
  }
  return(allps)
}


# for plotting

GEO$plot.poly = function(corners,add=FALSE,addnumbers=FALSE,...)
{
  optargs = list(...)
  if(!add)
  {
    xlim=optargs[["xlim"]]
    if(is.null(xlim))
    {
      xlim = range(corners[,1])
    }
    ylim= optargs[["ylim"]]
    if(is.null(ylim))
    {
      ylim = range(corners[,2])
    }
    plot(xlim,ylim,xlab="x",ylab="y",col="white")
  }
  polygon(corners[,1],corners[,2],...)
  if(addnumbers)
  {
    N = dim(corners)[1]
    ypos = rep(1,N)
    ypos[corners[,2]<mean(ylim)]=3
    text(corners[,1],corners[,2],labels=1:N,pos=ypos,cex=1,col="blue")
    points(corners[,1],corners[,2],pch=20,col="blue",cex=2)
    midxs = filter(corners[,1],filter=c(0.5,0.5),circular=T)
    midys = filter(corners[,2],filter=c(0.5,0.5),circular=T)
    points(midxs,midys,pch=20,col="red",cex=2)
    text(midxs,midys,labels=c(2:N,1),pos=ypos,cex=1,col="red")
  }
}


# end basic geo


#' Inverse distance interpolator
#' @description constructs a function that performs an inverse distance
#' interpolation in two  dimensions
#' @param tointerp a dataframe containing at least a columns called "x" and
#' "y" which will be used as coordinates of points in the plane
#' @param power default 2
#' @param zname the name of the column  to be interpolated, default "z"
#' @param  domain optional; should be a matrix of two column defining a polygon,
#' outside of this the reulting function wil return a value NA
#' @return a function that for any two arguments returns the interpolated value
#' @seealso \code{lininterpol.fun},\code{nearestneighbour.fun}
#' @export invdist.fun
#' @examples
#' N=100
#' x = runif(N)
#' y = runif(N)
#' W = abs(x-y*y)
#' ToInt = data.frame(x=x,y=y,W=W)
#' Npl = 35
#' xtopl = seq(0,1,length=Npl)
#' ytopl = seq(0,1,length=Npl)
#' IntFun = invdist.fun(ToInt,zname="W")
#' z = outer(xtopl,ytopl,IntFun)
#' image(xtopl,ytopl,z)
invdist.fun  = function (tointerp,power=2,zname="z",domain=NULL)
{
  foo = function(x,y)
  {
    w = 1/((tointerp$x-x)^2+(tointerp$y-y)^2)^(power/2)
    iexact = which(w==Inf)
    if(length(iexact)>0)
    {
      result=tointerp[iexact,zname]
    }
    else
    {
      result=sum(tointerp[,zname]*w)/sum(w)
    }
    if(!is.null(domain))
    {
      if(!GEO$inside.poly(domain,c(x,y)))
      {
        result=NA
      }
    }
    return(result)
  }
  return(Vectorize(foo))
}


#' Nearest neighbour interpolator
#' @description constructs a function
#' that performs a nearest neighbour interpolation in two  dimensions
#' @param data a dataframe containing at least a columns called "x" and
#' "y" which will be used as coordinates of points in the plane
#' @param zname the name of the column  to be interpolated, default "z"
#' @param  domain optional; should be a matrix of two column defining a polygon,
#' outside of this the reulting function wil return a value NA
#' @return a function that for any two arguments returns the interpolated value
#' @seealso \code{lininterpol.fun},\code{invdist.fun}
#' @export nearestneighbour.fun
#' @examples
#' N=100
#' x = runif(N)
#' y = runif(N)
#' W = abs(x-y*y)
#' ToInt = data.frame(x=x,y=y,W=W)
#' Npl = 35
#' xtopl = seq(0,1,length=Npl)
#' ytopl = seq(0,1,length=Npl)
#' IntFun = nearestneighbour.fun(ToInt,zname="W")
#' z = outer(xtopl,ytopl,IntFun)
#' image(xtopl,ytopl,z)
nearestneighbour.fun = function(data,zname="z",domain=NULL)
{
  foo= function(x,y)
  {
    point = c(x,y)
    i = which.min((x-data[,"x"])^2+(y-data[,"y"])^2)
    result = data[i,zname]
    if(!is.null(domain))
    {
      if(!GEO$inside.poly(domain,point))
      {
        result=NA
      }
    }
    return(result)
  }
  return(Vectorize(foo))
}


#'
#' Linear interpolator
#' @description constructs a function that performs a linear interpolation in
#' the traingles generated
#' by Delauney triangularization inside the convex hull given by the points.
#' Oustide of this convex hull an interpolation result calculated by the inverse
#' distance method will be returned.
#' @param tointerp a dataframe containing at least a columns called "x" and
#' "y" which will be used as coordinates of points in the plane
#' @param zname the name of the column  to be interpolated, default "z"
#' @param  domain optional; should be a matrix of two column defining a polygon,
#' outside of this the resulting function wil return a value NA
#' @return a function that for any two arguments returns the interpolated value
#' @seealso \code{invdist.fun}, \code{nearestneighbour.fun}
#' @export lininterpol.fun
#' @examples
#' N=100
#' x = runif(N)
#' y = runif(N)
#' W = abs(x-y*y)
#' ToInt = data.frame(x=x,y=y,W=W)
#' Npl = 35
#' xtopl = seq(0,1,length=Npl)
#' ytopl = seq(0,1,length=Npl)
#' IntFun = lininterpol.fun(ToInt,zname="W")
#' z = outer(xtopl,ytopl,IntFun)
#' image(xtopl,ytopl,z)
lininterpol.fun  = function (tointerp,zname="z",domain=NULL)
{
  #plot(tointerp[,"x"],tointerp["y"])
  D = deldir(tointerp[,c("x","y")])
  TrM = triMat(D)
  Ds =  D$delsgs
  ch = matrix(0,nrow=0,ncol=6)
  for(i in 1:nrow(Ds))
  {
    linenodes = Ds[i,c("ind1","ind2")]
    count = 0
    for(k in 1:nrow(TrM))
    {
      if(sum(is.element(linenodes,TrM[k,]))==2)
      {
        count=count+1
      }
    }
    if(count==1)
    {
      ch = rbind(ch,Ds[i,])
    }
  }
  Np = dim(tointerp)[1]
  ListOfTr = lapply(1:Np, function(i){which((TrM[,1]==i)|(TrM[,2]==i)|(TrM[,3]==i))})
  Trf = matrix(0,nrow=dim(TrM)[1],ncol=12)
  for(i in 1:dim(TrM)[1])
  {
    p1 = c(tointerp[[TrM[i,1],"x"]],tointerp[[TrM[i,1],"y"]])
    Z1 = tointerp[[TrM[i,1],zname]]
    p2 = c(tointerp[[TrM[i,2],"x"]],tointerp[[TrM[i,2],"y"]])
    Z2 = tointerp[[TrM[i,2],zname]]
    p3 = c(tointerp[[TrM[i,3],"x"]],tointerp[[TrM[i,3],"y"]])
    Z3 = tointerp[[TrM[i,3],zname]]
    f1o = p2[1]*p3[2]-p3[1]*p2[2]
    f1x = p2[2]-p3[2]
    f1y = p3[1]-p2[1]
    A = f1o+f1x*p1[1]+f1y*p1[2]
    f2o = p3[1]*p1[2]-p1[1]*p3[2]
    f2x = p3[2]-p1[2]
    f2y = p1[1]-p3[1]
    f3o = p1[1]*p2[2]-p2[1]*p1[2]
    f3x = p1[2]-p2[2]
    f3y = p2[1]-p1[1]
    Trf[i,]=c(c(f1o,f1x,f1y,f2o,f2x,f2y,f3o,f3x,f3y)/A,Z1,Z2,Z3)
  }

  foo = function(x,y)
  {
    point = c(x,y)
    result = NA
    findtriangle = TRUE
    if(!is.null(domain))
    {
      if(!GEO$inside.poly(domain,point))
      {
        findtriangle = FALSE
      }
    }
    doftriangle = function(fvec)
    {
      if(findtriangle)
      {
        f1 = fvec[1]+fvec[2]*x+fvec[3]*y
        f2 = fvec[4]+fvec[5]*x+fvec[6]*y
        f3 = fvec[7]+fvec[8]*x+fvec[9]*y
        if((f1>0)&(f2>0)&(f3>0))
        {
          result<<- f1*fvec[10]+f2*fvec[11]+f3*fvec[12]
          findtriangle = FALSE
        }
      }
    }
    dummy = apply(Trf,1,doftriangle)
    result = as.numeric(result)
    if(is.na(result)&(findtriangle))
    {
      l =  ((ch[,"x1"]-ch[,"x2"])*(x-ch[,"x2"])+
              (ch[,"y1"]-ch[,"y2"])*(y-ch[,"y2"]))/
        ((ch[,"x1"]-ch[,"x2"])^2+(ch[,"y1"]-ch[,"y2"])^2)
      l[l<0] = 0
      l[l>1] = 1
      xp = l*ch[,"x1"]+(1-l)*ch[,"x2"]
      yp = l*ch[,"y1"]+(1-l)*ch[,"y2"]
      d = (xp-x)^2+(y-yp)^2
      imin = which.min(d)
      result = l[imin]*tointerp[ch[imin,"ind1"],zname]+
        (1-l[imin])*tointerp[ch[imin,"ind2"],zname]
    }
    return(result)
  }
  return(Vectorize(foo))
}
#
# # some examples:
# N= 100
# x = runif(N)
# y = runif(N)
# W = 4*abs(x-y*y)
#
# ToInt = data.frame(x=x,y=y,W=W)
#
# Npl = 35
# xtopl = seq(0,1,length=Npl)
# ytopl = seq(0,1,length=Npl)
#
#
# LinIntFun = lininterpol.fun(ToInt,zname="W")
# z = outer(xtopl,ytopl,LinIntFun)
# image(x=xtopl,y=ytopl,z)
# points(ToInt[,"x"],ToInt[,"y"])
#
#
# domain = matrix(c(0,0,0.6,0.6,1,0,0.2,0.9),ncol=2)
# LinIntFun = lininterpol.fun(ToInt,zname="W",domain=domain)
# z = outer(xtopl,ytopl,LinIntFun)
# image(x=xtopl,y=ytopl,z)
# points(ToInt[,"x"],ToInt[,"y"])
#



