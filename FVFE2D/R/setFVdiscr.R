index.in.mat  = function(mat,x,y)
{
  which(apply(mat,1,
              function(r){(r[1]==x)&(r[2]==y)}))
}

add2DFVdiscr_rectgeneral = function(model,base.x,base.y)
{
  nodes = matrix(0,nrow=0,ncol=2) # the nodes for the balances

  extend.over.range = function(vec,range)
  {
    dv = vec[2]-vec[1]
    while(vec[1] >= range[1])
    {
      vec = c(vec[1]-dv,vec)
    }
    dv = vec[length(vec)]-vec[length(vec)-1]
    while(vec[length(vec)]<= range[2])
    {
      vec = c(vec,vec[length(vec)]+dv)
    }
    return(vec)
  }
  base.x = extend.over.range(base.x,range(model$domain[,1]))
  base.y = extend.over.range(base.y,range(model$domain[,2]))

  Nbx = length(base.x)
  Nby = length(base.y)

  all.xy = matrix(0,nrow=Nbx*Nby,ncol=2)
  all.xy[,1] = rep(base.x,Nby)
  all.xy[,2] = rep(base.y,each =Nbx)

  in.model.domain = function(xy)
  {
    return(GEO$inside.poly(model$sydomain,model$doscale(c(xy[1],xy[2]))))
  }

  IN.xy = apply(all.xy,MARGIN = 1,FUN=in.model.domain)
  model$nodes = all.xy[IN.xy,]
  model$Numnodes = nrow(model$nodes)
  ind.nodes = cumsum(IN.xy)
  nodes.ind = (1:nrow(all.xy))[IN.xy]

  cellgeo = list()
  cellinterfaces = matrix(0,nrow=0,ncol=10)
  boundarypart = matrix(0,nrow=0,ncol=11)

  for(k in 1:model$Numnodes)
  {
    i = nodes.ind[k]
    iN = i-(Nbx)
    iS = i+(Nbx)
    iW = i-1
    iE = i+1
    xy.NE = (all.xy[iN,]+all.xy[iE,])/2
    xy.SE = (all.xy[iS,]+all.xy[iE,])/2
    xy.NW = (all.xy[iN,]+all.xy[iW,])/2
    xy.SW = (all.xy[iS,]+all.xy[iW,])/2
    xy.cell= rbind(xy.NE,xy.SE,xy.SW,xy.NW)

    cellgeo[[k]] = xy.cell

    if(IN.xy[iN])
    {
      # make cell interface
      at = (xy.NW+xy.NE)/2
      i.to = ind.nodes[iN]
      nodedist = sqrt(sum((model$nodes[k,]-model$nodes[i.to,])^2))
      length = sqrt(sum((xy.NW-xy.NE)^2))
      cellinterfaces=rbind(cellinterfaces,c(k,i.to,
                                            xy.NW[1],xy.NW[2],xy.NE[1],xy.NE[2],
                                            at[1],at[2],length,nodedist))
    } else {
      # make boundary
      at = (xy.NW+xy.NE)/2
      length = sqrt(sum((xy.NW-xy.NE)^2))
      if(length<0) {browser()}
      normal = c(0,-1)
      isegnum = GEO$isegnum(model$sydomain,model$doscale(at))
      boundarypart=rbind(boundarypart,
                         c(k,length,at[1],at[2],
                           normal[1],normal[2],isegnum,
                           xy.NW[1],xy.NW[2],xy.NE[1],xy.NE[2]))
    }
    if(IN.xy[iW])
    {
      # make cell interface
      at = (xy.SW+xy.NW)/2
      i.to = ind.nodes[iW]
      nodedist = sqrt(sum((model$nodes[k,]-model$nodes[i.to,])^2))
      length = sqrt(sum((xy.SW-xy.NW)^2))
      cellinterfaces=rbind(cellinterfaces,c(k,i.to,
                                            xy.SW[1],xy.SW[2],xy.NW[1],xy.NW[2],
                                            at[1],at[2],length,nodedist))

    } else {
      # make boundary
      at = (xy.NW+xy.SW)/2
      length = sqrt(sum((xy.NW-xy.SW)^2))
      if(length<0) {browser()}
      normal = c(1,0)
      isegnum = GEO$isegnum(model$sydomain,model$doscale(at))
      boundarypart=rbind(boundarypart,
                         c(k,length,at[1],at[2],
                           normal[1],normal[2],isegnum,
                           xy.SW[1],xy.SW[2],xy.NW[1],xy.NW[2]))
    }
    if(!IN.xy[iS])
    {
      # make boundary
      at = (xy.SW+xy.SE)/2
      length = sqrt(sum((xy.SW-xy.SE)^2))
      if(length<0) {browser()}
      normal = c(0,1)
      isegnum = GEO$isegnum(model$sydomain,model$doscale(at))
      boundarypart=rbind(boundarypart,
                         c(k,length,at[1],at[2],
                           normal[1],normal[2],isegnum,
                           xy.SE[1],xy.SE[2],xy.SW[1],xy.SW[2]))
    }
    if(!IN.xy[iE])
    {
      # make boundary
      at = (xy.SE+xy.NE)/2
      length = sqrt(sum((xy.SE-xy.NE)^2))
      if(length<0) {browser()}
      normal = c(-1,0)
      isegnum = GEO$isegnum(model$sydomain,model$doscale(at))
      boundarypart=rbind(boundarypart,
                         c(k,length,at[1],at[2],
                           normal[1],normal[2],isegnum,
                           xy.NE[1],xy.NE[2],xy.SE[1],xy.SE[2]))
    }
  }

  colnames(boundarypart)=c("nid","length","atx","aty","normalx","normaly",
                           "domainsegid","x1","y1","x2","y2")
  colnames(cellinterfaces)=c("ni1","ni2","x1","y1","x2","y2",
                             "x","y","facelength","nodedistance")

  model$cellgeo  = cellgeo
  model$cellinterfaces = cellinterfaces
  model$boundarypart = boundarypart

}


add2DFVdiscr_rectregular = function(model,dx,dy)
{
  y = seq(model$ymin,model$ymin+model$scale,by=dy)
  x = seq(model$xmin-model$scale*pi/4,model$xmin+model$scale,by=dx)
  y = seq(model$ymin-model$scale*pi/4,model$ymin+model$scale,by=dy)
  add2DFVdiscr_rectgeneral(model,x,y)
}

add2DFVdiscr_hexagonal = function(model,side)
{
  nodes = matrix(0,nrow=0,ncol=2)
  cnodes = matrix(0,nrow=0,ncol=2)
  cellgeo = list()
  cellinterfaces = matrix(0,nrow=0,ncol=10)
  boundarypart = matrix(0,nrow=0,ncol=11)

  heighty = side*sqrt(3)/2
  h = heighty
  s = side
  normfac = sqrt(h^2+s^2/4)

  NN = c(0,2*h/3)
  NE = c(s/2,h/3)
  SE = c(s/2,-h/3)
  SS = c(0,-2*h/3)
  SW = c(-s/2,-h/3)
  NW = c(-s/2,h/3)
  ALLx = c(NN[1],NE[1],SE[1],SS[1],SW[1],NW[1])
  ALLy = c(NN[2],NE[2],SE[2],SS[2],SW[2],NW[2])

  index.nodes  = function(x,y)
  {
    l = which(apply(nodes,1,
                    function(r){(r[1]-x)^2+(r[2]-y)^2<1e-5}))
    if(length(l)>0)
    {
      return(l[1])
    }
    else
    {
      return(NA)
    }
  }

  index.cnodes  = function(x,y)
  {
    l = which(apply(cnodes,1,
                    function(r){(r[1]-x)^2+(r[2]-y)^2<1e-5}))
    if(length(l)>0)
    {
      return(l[1])
    }
    else
    {
      cnodes<<-rbind(cnodes,c(x,y))
      return(dim(cnodes)[1])
    }
  }

  do.newnode = function(x,y)
  {
    nodes <<- rbind(nodes,c(x,y))
    cell = matrix(0,nrow=6,ncol=2)
    for(i in 1:6)
    {
      cell[i,1]=x+ALLx[i]
      cell[i,2]=y+ALLy[i]
    }
    cellgeo[[length(cellgeo)+1]] <<- cell
  }
  y = model$ymin-model$scale*pi/4
  while(y< model$ymin+model$scale)
  {
    x = model$xmin-side/2-model$scale*pi/4
    while(x < model$xmin+model$scale)
    {
      newp = c(x,y)
      if(GEO$inside.poly(model$sydomain,model$doscale(newp)))
      {
        do.newnode(x,y)
      }
      x = x+side
    }
    y = y + heighty
    x = model$xmin-model$scale*pi/4
    while(x<model$xmin+model$scale)
    {
      newp = c(x,y)
      if(GEO$inside.poly(model$sydomain,model$doscale(newp)))
      {
        do.newnode(x,y)
      }
      x = x+side
    }
    y = y + heighty
  }
  # making cell interfaces
  for(i in 1:dim(nodes)[1])
  {
    xi = nodes[i,1]
    yi = nodes[i,2]
    iNE = index.cnodes(xi+NE[1],yi+NE[2])
    iNN = index.cnodes(xi+NN[1],yi+NN[2])
    iSE = index.cnodes(xi+SE[1],yi+SE[2])
    iSS = index.cnodes(xi+SS[1],yi+SS[2])
    iSW = index.cnodes(xi+SW[1],yi+SW[2])
    iNW = index.cnodes(xi+NW[1],yi+NW[2])
    j = index.nodes(xi+s/2,yi+h)
    if(!is.na(j))
    {
      atx = (cnodes[iNE,1]+cnodes[iNN,1])/2
      aty = (cnodes[iNE,2]+cnodes[iNN,2])/2
      length = sqrt((cnodes[iNE,1]-cnodes[iNN,1])^2+
                      (cnodes[iNE,2]-cnodes[iNN,2])^2)
      nodedist= sqrt((nodes[i,1]-nodes[j,1])^2+
                       (nodes[i,2]-nodes[j,2])^2)

      cellinterfaces = rbind(cellinterfaces,c(i,j,
                                              cnodes[iNE,1],cnodes[iNE,2],
                                              cnodes[iNN,1],cnodes[iNN,2],
                                              atx,aty,length,nodedist))
    } else {
      at = (cnodes[iNE,]+cnodes[iNN,])/2
      length = sqrt((cnodes[iNE,1]-cnodes[iNN,1])^2+
                      (cnodes[iNE,2]-cnodes[iNN,2])^2)
      isegnum = GEO$isegnum(model$sydomain,model$doscale(nodes[i,]))
      normal = c(-s/2,-h)/normfac
      boundarypart=rbind(boundarypart,
                         c(i,length,at[1],at[2],
                           normal[1],normal[2],isegnum,
                           cnodes[iNE,1],cnodes[iNE,2],
                           cnodes[iNN,1],cnodes[iNN,2]))
    }

    j = index.nodes(xi+s,yi)
    if(!is.na(j))
    {
      atx = (cnodes[iNE,1]+cnodes[iSE,1])/2
      aty = (cnodes[iNE,2]+cnodes[iSE,2])/2
      length = sqrt((cnodes[iNE,1]-cnodes[iSE,1])^2+
                      (cnodes[iNE,2]-cnodes[iSE,2])^2)
      nodedist= sqrt((nodes[i,1]-nodes[j,1])^2+
                       (nodes[i,2]-nodes[j,2])^2)

      cellinterfaces = rbind(cellinterfaces,c(i,j,
                                              cnodes[iNE,1],cnodes[iNE,2],
                                              cnodes[iSE,1],cnodes[iSE,2],
                                              atx,aty,length,nodedist))
    } else {
      at = (cnodes[iNE,]+cnodes[iSE,])/2
      length = sqrt((cnodes[iNE,1]-cnodes[iSE,1])^2+
                      (cnodes[iNE,2]-cnodes[iSE,2])^2)
      isegnum = GEO$isegnum(model$sydomain,model$doscale(nodes[i,]))
      normal = c(-1,0)
      boundarypart=rbind(boundarypart,
                         c(i,length,at[1],at[2],
                           normal[1],normal[2],isegnum,
                           cnodes[iNE,1],cnodes[iNE,2],
                           cnodes[iSE,1],cnodes[iSE,2]))
    }
    j = index.nodes(xi+s/2,yi-h)
    if(!is.na(j))
    {
      atx = (cnodes[iSE,1]+cnodes[iSS,1])/2
      aty = (cnodes[iSE,2]+cnodes[iSS,2])/2
      length = sqrt((cnodes[iSE,1]-cnodes[iSS,1])^2+
                      (cnodes[iSE,2]-cnodes[iSS,2])^2)
      nodedist= sqrt((nodes[i,1]-nodes[j,1])^2+
                       (nodes[i,2]-nodes[j,2])^2)

      cellinterfaces = rbind(cellinterfaces,c(i,j,
                                              cnodes[iSE,1],cnodes[iSE,2],
                                              cnodes[iSS,1],cnodes[iSS,2],
                                              atx,aty,length,nodedist))
    }  else {
      at = (cnodes[iSS,]+cnodes[iSE,])/2
      length = sqrt((cnodes[iSS,1]-cnodes[iSE,1])^2+
                      (cnodes[iSS,2]-cnodes[iSE,2])^2)
      isegnum = GEO$isegnum(model$sydomain,model$doscale(nodes[i,]))
      normal = c(-s/2,h)/normfac
      boundarypart=rbind(boundarypart,
                         c(i,length,at[1],at[2],
                           normal[1],normal[2],isegnum,
                           cnodes[iSE,1],cnodes[iSE,2],
                           cnodes[iSS,1],cnodes[iSS,2]))
    }
    j = index.nodes(xi-s/2,yi-h)
    if(is.na(j))
    {
      at = (cnodes[iSS,]+cnodes[iSW,])/2
      length = sqrt((cnodes[iSS,1]-cnodes[iSW,1])^2+
                      (cnodes[iSS,2]-cnodes[iSW,2])^2)
      isegnum = GEO$isegnum(model$sydomain,model$doscale(nodes[i,]))
      normal = c(s/2,h)/normfac
      boundarypart=rbind(boundarypart,
                         c(i,length,at[1],at[2],
                           normal[1],normal[2],isegnum,
                           cnodes[iSS,1],cnodes[iSS,2],
                           cnodes[iSW,1],cnodes[iSW,2]))
    }
    j = index.nodes(xi-s,yi)
    if(is.na(j))
    {
      at = (cnodes[iNW,]+cnodes[iSW,])/2
      length = sqrt((cnodes[iNW,1]-cnodes[iSW,1])^2+
                      (cnodes[iNW,2]-cnodes[iSW,2])^2)
      isegnum = GEO$isegnum(model$sydomain,model$doscale(nodes[i,]))
      normal = c(1,0)
      boundarypart=rbind(boundarypart,
                         c(i,length,at[1],at[2],
                           normal[1],normal[2],isegnum,
                           cnodes[iNW,1],cnodes[iNW,2],
                           cnodes[iSW,1],cnodes[iSW,2]))
    }
    j = index.nodes(xi-s/2,yi+h)
    if(is.na(j))
    {
      at = (cnodes[iNW,]+cnodes[iNN,])/2
      length = sqrt((cnodes[iNW,1]-cnodes[iNN,1])^2+
                      (cnodes[iNW,2]-cnodes[iNN,2])^2)
      isegnum = GEO$isegnum(model$sydomain,model$doscale(nodes[i,]))
      normal = c(s/2,-h)/normfac
      boundarypart=rbind(boundarypart,
                         c(i,length,at[1],at[2],
                           normal[1],normal[2],isegnum,
                           cnodes[iNW,1],cnodes[iNW,2],
                           cnodes[iNN,1],cnodes[iNN,2]))
    }
  }
  colnames(cellinterfaces)=c("ni1","ni2","x1","y1","x2","y2",
                             "x","y","facelength","nodedistance")
  colnames(boundarypart)=c("nid","length","atx","aty","normalx","normaly",
                           "domainsegid","x1","y1","x2","y2")
  model$nodes = nodes
  model$cellgeo = cellgeo
  model$cellinterfaces = cellinterfaces
  model$boundarypart = boundarypart
}

add2DFVdiscr_unstructured = function(model,nodes)
{
  snodes = model$doscale(nodes)

  Iok = c()
  for(i in 1:nrow(snodes))
  {
    if(GEO$inside.poly(model$sydomain,snodes[i,]))
    {
      Iok = c(Iok,i)
    }
    else if(GEO$dist.poly.point(model$sydomain,snodes[i,]) <= 0.01)
    {
      Iok=c(Iok,i)
    }
  }

  model$nodes = nodes[Iok,]
  snodes = snodes[Iok,]

  # make voronoi

  rw = c(min(snodes[,1],model$sydomain[,1]),max(snodes[,1],model$sydomain[,1]),
         min(snodes[,2],model$sydomain[,2]),max(snodes[,2],model$sydomain[,2]))
  rw = c(1.1*rw[1]-0.1*rw[2],-0.1*rw[1]+1.1*rw[2], 1.1*rw[3]-0.1*rw[4],-0.1*rw[3]+1.1*rw[4])
  d = deldir(snodes[,1],snodes[,2],eps=1e-6,rw=rw)

  if(is.null(d)) # zonder genade geen verlossing
  {
    d = deldir(snodes[,1],snodes[,2],eps=1e-8)
  }
  if(is.null(d))
  {
    stop("damn deldir")
  }

  Ndom = nrow(model$sydomain)
  Nnod = nrow(nodes)

  model$cellinterfaces  = matrix(0,nrow=0,ncol=10)

  cellsides = list()
  for(i in 1:Nnod)
  {
    cellsides[[i]] = matrix(0,nrow=0,ncol=5)
    colnames(cellsides[[i]]) = c("x1","y1","x2","y2","segnum1")
  }


  for(i in 1:dim(d$dirsgs)[1])
  {
    nid1 = d$dirsgs[i,"ind1"]
    nid2 = d$dirsgs[i,"ind2"]
    seg = matrix(unlist(d$dirsgs[i,1:4]),ncol=2,byrow=TRUE)
    clippedseg = GEO$clip.segment.in.poly(model$sydomain,seg)

    if(!is.null(clippedseg))
    {
      usegs = model$dounscale(clippedseg[,1:2])
      xnewside = NULL
      if(clippedseg[1,3]>0)
      {
        newside = c(usegs[1,1],usegs[1,2],
                    usegs[2,1],usegs[2,2],
                    clippedseg[1,3])
        if(clippedseg[2,3]>0)
        {
          xnewside = c(usegs[2,1],usegs[2,2],
                       usegs[2,1],usegs[2,2],
                       clippedseg[2,3])
        }
      } else {
        newside = c(usegs[2,1],usegs[2,2],
                    usegs[1,1],usegs[1,2],
                    clippedseg[2,3])
      }


      cellsides[[nid1]] = rbind(cellsides[[nid1]],newside)
      cellsides[[nid2]] = rbind(cellsides[[nid2]],newside)
      if(!is.null(xnewside))
      {
        cellsides[[nid1]] = rbind(cellsides[[nid1]],xnewside)
        cellsides[[nid2]] = rbind(cellsides[[nid2]],xnewside)
      }

      at = (usegs[1,]+usegs[2,])/2
      length = sqrt(sum((usegs[2,]-usegs[1,])^2))
      nodedist = sqrt(sum((nodes[nid1,]-nodes[nid2,])^2))
      model$cellinterfaces = rbind(model$cellinterfaces,
                                   c(nid1,nid2,
                                     usegs[1,1],usegs[1,2],
                                     usegs[2,1],usegs[2,2],
                                     at[1],at[2],length,nodedist))
    }
  }


  # boundary registration
  model$boundarypart = matrix(0,nrow=0,ncol=11)

  newboundarypart = function(nodei,segi,x1,y1,x2,y2)
  {
    #segnum = GEO$isegnum(model$sydomain,model$doscale(model$nodes[nodei,]))
    segnum = segi
    length = max(sqrt((x1-x2)^2+(y1-y2)^2),1e-10)
    atx = (x1+x2)/2
    aty = (y1+y2)/2
    normalx = -(y1-y2)/length
    normaly = (x1-x2)/length
    testp = model$doscale(c(atx,aty))+5*GEO$eps*c(normalx,normaly)

    if(!GEO$inside.poly(model$sydomain,testp))
    {
      normalx = - normalx
      normaly = - normaly
    }
    model$boundarypart <<-rbind(model$boundarypart,
                                c(nodei,length,atx,aty,normalx,normaly,segnum,
                                  x1,y1,x2,y2))
    newside = c(x1,y1,x2,y2,segnum)
    cellsides[[nodei]] <<- rbind(cellsides[[nodei]],newside)
  }



  # find boundary nodes in cells

  TL = tile.list(d)
  NTL = length(TL)
  ck = rgb(runif(NTL),runif(NTL),runif(NTL),0.5)

  domainnodesincell = rep(-1,length(model$sydomain))

  for(kd in 1:Ndom)
  {
    kT = 1
    while(domainnodesincell[kd] < 0)
    {
      polmat = cbind(TL[[kT]]$x,TL[[kT]]$y)
      if(GEO$inside.poly(polmat,model$sydomain[kd,]))
      {
        domainnodesincell[kd]=kT
      }
      kT = kT+1
    }
  }

  # add boundaries
  for(i in 1:Nnod)
  {
    cs = cellsides[[i]]
    segnums = cs[,"segnum1"]
    segnumsx = unique(segnums[segnums>0])
    for(s in segnumsx)
    {
      posx = which(segnums==s)
      if(length(posx)==2)
      {
        newboundarypart(i,s,cs[posx[1],1],cs[posx[1],2],
                        cs[posx[2],1],cs[posx[2],2])
      } else if(length(posx==1))
      {
        dnode = s
        if(domainnodesincell[dnode]==i)
        {
          newboundarypart(i,s,cs[posx[1],1],cs[posx[1],2],
                          model$domain[dnode,1],model$domain[dnode,2])
        } else {
          dnode = s%%Ndom+1
          if(domainnodesincell[dnode]==i)
          {
            newboundarypart(i,s,cs[posx[1],1],cs[posx[1],2],
                            model$domain[dnode,1],model$domain[dnode,2])
          }
        }
      }
    }
  }

  # add whole domain segments in cells
  for(j in 1:Ndom)
  {
    ic = domainnodesincell[[j]]
    nextj = j%%Ndom+1

    icn = domainnodesincell[[nextj]]
    if(ic==icn)
    {
      newboundarypart(ic,j,model$domain[j,1],model$domain[j,2],
                      model$domain[nextj,1],model$domain[nextj,2])

    }

  }

  makecell = function(segments)
  {
    cell = segments[1,]
    availrows = 2:nrow(segments)
    lastx = segments[1,3]
    lasty = segments[1,4]
    while(length(availrows)>0)
    {
      findin1 = (segments[availrows,1]-lastx)^2+
        (segments[availrows,2]-lasty)^2
      findin2 = (segments[availrows,3]-lastx)^2+
        (segments[availrows,4]-lasty)^2
      rowf1 = which.min(findin1)
      fmin1 = findin1[rowf1]
      rowf2 = which.min(findin2)
      fmin2 = findin2[rowf2]

      if(fmin1<fmin2)
      {
        linerow = availrows[rowf1]
        newrow  = segments[linerow,c(1,2,3,4)]

      } else {
        linerow = availrows[rowf2]
        newrow = segments[linerow,c(3,4,1,2)]
      }
      cell = rbind(cell,newrow)
      lastx = newrow[3]
      lasty = newrow[4]
      availrows = setdiff(availrows,linerow)

    }
    return(cell)
  }

  model$cellgeo = list()
  for(i in 1:length(cellsides))
  {
    tcell = cellsides[[i]][,c("x1","y1","x2","y2")]

    tcell2 = subset(tcell,rownames(tcell)=="newside")

    model$cellgeo[[i]] = makecell(tcell2)

  }
  colnames(model$cellinterfaces)=c("ni1","ni2","x1","y1","x2","y2",
                                   "x","y","facelength","nodedistance")
  colnames(model$boundarypart)=c("nid","length","atx","aty","normalx","normaly",
                                 "domainsegid","x1","y1","x2","y2")
}
