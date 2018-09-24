#source("GEO.R")


add2DFEdiscr_rectregular = function(model,dx,dy)
{
  x = seq(model$xmin-model$scale*pi/4,model$xmin+model$scale,by=dx)
  y = seq(model$ymin-model$scale*pi/4,model$ymin+model$scale,by=dy)
  add2DFEdiscr_rectgeneral(model,x,y)
}

add2DFEdiscr_rectgeneral = function(model,bx,by)
{
  nodes = matrix(0,nrow=0,ncol=2)
  triangles = matrix(0,nrow=0,ncol=3)

  nodes.index = function(x,y)
  {
    l = which(apply(nodes,1,
                    function(r){(r[1]-x)^2+(r[2]-y)^2<1e-5}))
    if(length(l)>0)
    {
      return(l[1])
    }
    else
    {
      nodes <<- rbind(nodes,c(x,y))
      return(nrow(nodes))
    }
  }

  In = c()

  for(i in 2:length(bx))
  {
    for(j in 2:length(by))
    {
      cx = c(bx[i-1],bx[i-1],bx[i],bx[i])
      cy = c(by[j-1],by[j],by[j],by[j-1])
      for(k in 1:4)
      {
        In[k] = GEO$inside.poly(model$sydomain,
                                model$doscale(matrix(c(cx[k],cy[k]),ncol=2)))
      }
      S = sum(In)
      if(S==3)
      {
        Iok = which(In[]==1)
        ni = c()
        for(k  in 1:3)
        {
          ni[k] = nodes.index(cx[Iok[k]],cy[Iok[k]])
        }
        triangles = rbind(triangles,ni)
      } else if(S==4)
      {
        Iok = c(1,2,3)
        ni = c()
        for(k  in 1:3)
        {
          ni[k] = nodes.index(cx[Iok[k]],cy[Iok[k]])
        }
        triangles = rbind(triangles,ni)
        Iok = c(3,4,1)
        for(k  in 1:3)
        {
          ni[k] = nodes.index(cx[Iok[k]],cy[Iok[k]])
        }
        triangles = rbind(triangles,ni)
      }
    }
  }
  sides = matrix(0,ncol=3,nrow=0)

  find.side = function(k1,k2)
  {
    return(which(apply(sides,1,function(r){
      ((r[1]==k1)&(r[2]==k2))|((r[1]==k2)&(r[2]==k1))})))
  }

  for(k in 1:nrow(triangles))
  {
    k1 = triangles[k,1]
    k2 = triangles[k,2]
    l = find.side(k1,k2)
    if(length(l)==0)
    {
      sides = rbind(sides,c(k1,k2,1))
    } else {
      sides[l,3] = 2
    }
    k1 = triangles[k,2]
    k2 = triangles[k,3]
    l = find.side(k1,k2)
    if(length(l)==0)
    {
      sides = rbind(sides,c(k1,k2,1))
    } else {
      sides[l,3] = 2
    }
    k1 = triangles[k,3]
    k2 = triangles[k,1]
    l = find.side(k1,k2)
    if(length(l)==0)
    {
      sides = rbind(sides,c(k1,k2,1))
    } else {
      sides[l,3] = 2
    }
  }
  borders = sides[sides[,3]==1,]
  for(k in 1:nrow(borders))
  {
    unscaledmidp = (nodes[borders[k,1],]+nodes[borders[k,2],])/2
    midp = model$doscale(unscaledmidp)
    borders[k,3]= GEO$isegnum(model$sydomain,midp)
  }
  colnames(triangles)=NULL
  model$nodes = nodes
  model$elements  = triangles
  model$boundarypart = borders
}

add2DFEdiscr_hexagonal = function(model,side)
{
  s = side/model$scale
  h = s*sqrt(3)/2

  nodes = matrix(0,nrow=0,ncol=2)

  nodes.index = function(x,y)
  {

    l = which(apply(nodes,1,
                    function(r){(r[1]-x)^2+(r[2]-y)^2 < 1e-8}))
    if(length(l)>0)
    {
      return(l[1])
    }
    else
    {
      nodes <<- rbind(nodes,c(x,y))
      return(nrow(nodes))
    }
  }

  In= c()
  x = -s-pi/400
  triangles = matrix(0,nrow=0,ncol=3)
  while(x<1)
  {
    y = -s-pi/400
    while(y<1)
    {
      cx = c(x,x+s/2,x+s,x+s/2,x+3*s/2,x+3*s/2)
      cy = c(y,y+h,y,y-h,y+h,y-h)
      for(k in 1:6)
      {
        In[k] = GEO$inside.poly(model$sydomain,
                                matrix(c(cx[k],cy[k]),ncol=2))
      }
      if(In[1]&In[2]&In[3])
      {
        ni = c()
        ni[1] = nodes.index(cx[1],cy[1])
        ni[2] = nodes.index(cx[2],cy[2])
        ni[3] = nodes.index(cx[3],cy[3])
        triangles = rbind(triangles,ni)

      }
      if(In[1]&In[3]&In[4])
      {
        ni = c()
        ni[1] = nodes.index(cx[1],cy[1])
        ni[2] = nodes.index(cx[3],cy[3])
        ni[3] = nodes.index(cx[4],cy[4])
        triangles = rbind(triangles,ni)

      }
      if(In[2]&In[3]&In[5])
      {
        ni = c()
        ni[1] = nodes.index(cx[2],cy[2])
        ni[2] = nodes.index(cx[3],cy[3])
        ni[3] = nodes.index(cx[5],cy[5])
        triangles = rbind(triangles,ni)

      }
      if(In[3]&In[4]&In[6])
      {
        ni = c()
        ni[1] = nodes.index(cx[3],cy[3])
        ni[2] = nodes.index(cx[4],cy[4])
        ni[3] = nodes.index(cx[6],cy[6])
        triangles = rbind(triangles,ni)

      }
      y=y+2*h
    }
    x=x+s

  }

  sides = matrix(0,ncol=3,nrow=0)

  find.side = function(k1,k2)
  {
    return(which(apply(sides,1,function(r){
      ((r[1]==k1)&(r[2]==k2))|((r[1]==k2)&(r[2]==k1))})))
  }
  for(k in 1:nrow(triangles))
  {
    k1 = triangles[k,1]
    k2 = triangles[k,2]
    l = find.side(k1,k2)
    if(length(l)==0)
    {
      sides = rbind(sides,c(k1,k2,1))
    } else {
      sides[l,3] = 2
    }
    k1 = triangles[k,2]
    k2 = triangles[k,3]
    l = find.side(k1,k2)
    if(length(l)==0)
    {
      sides = rbind(sides,c(k1,k2,1))
    } else {
      sides[l,3] = 2
    }
    k1 = triangles[k,3]
    k2 = triangles[k,1]
    l = find.side(k1,k2)
    if(length(l)==0)
    {
      sides = rbind(sides,c(k1,k2,1))
    } else {
      sides[l,3] = 2
    }
  }

  borders = sides[sides[,3]==1,]
  for(k in 1:nrow(borders))
  {
    midp = (nodes[borders[k,1],]+nodes[borders[k,2],])/2
    midp[2] = midp[2]*model$ytoxratio
    borders[k,3]= GEO$isegnum(model$sydomain,midp)
  }

  model$nodes = model$dounscale(nodes)
  model$elements  = triangles
  model$boundarypart = borders

}

add2DFEdiscr_unstructured = function(model,nodes)
{
  d = deldir(nodes[,1],nodes[,2])
  trs = triMat(d)
  sides = d$delsgs[,c("ind1","ind2")]
  cnts = rep(0,nrow(sides))
  do_tri = function(t)
  {
    result = FALSE
    pmid = colMeans(nodes[t,])
    if(GEO$inside.poly(model$domain,pmid))
    {
      ks = sort(c(t[1],t[2]))
      i = which(sides[,1]==ks[2]&sides[,2]==ks[1])
      cnts[i]<<-cnts[i]+1
      ks = sort(c(t[1],t[3]))
      i = which(sides[,1]==ks[2]&sides[,2]==ks[1])
      cnts[i]<<-cnts[i]+1
      ks = sort(c(t[2],t[3]))
      i = which(sides[,1]==ks[2]&sides[,2]==ks[1])
      cnts[i]<<-cnts[i]+1
      result=TRUE
    }
    return(result)
  }

  trisOK = apply(trs,1,do_tri)

  triangles = trs[trisOK,]
  borders = as.matrix(sides[cnts==1,])
  borders=cbind(borders,rep(0,nrow(borders)))
  for(k in 1:nrow(borders))
  {
    midp = model$doscale((nodes[borders[k,1],]+nodes[borders[k,2],])/2)
    borders[k,3]= as.numeric(GEO$isegnum(model$sydomain,midp))
  }
  model$nodes = nodes
  model$elements  = triangles
  model$boundarypart = borders
}
