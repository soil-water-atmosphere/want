library(lhs)
library(truncnorm)


GaussianLHS = function(samplesize,mean,sd,varnames=NULL,
                       lower=NULL,upper=NULL)
{
  N = length(mean)
  if(N < 1)
  {
    cat("error mean should have  length > 0\n")
    return()
  }
  if(length(sd)!=N)
  {
    cat("error mean and sd should have equal length\n")
    return()
  }
  if(is.null(varnames))
  {
    varnames=paste("X",1:N,sep="")
  }
  if(length(varnames)!=N)
  {
    cat("error names and mean should have equal length\n")
    return()
  }
  if(is.null(lower))
  {
    lower = rep(-Inf,N)
  }
  if(is.null(upper))
  {
    upper = rep(Inf,N)
  }
  result = data.frame(improvedLHS(samplesize,N))
  names(result) = varnames
  for(k in 1:N)
  {
    result[,k] =  qtruncnorm(result[,k],a=lower[k],b=upper[k],mean[k],sd[k]) 
  }
  return(result)
}


linloess = function(formula,data)
{
  tf = attr(terms(formula),"variables")
  ltf = as.list(tf)
  vvars = c()
  for(i in 2:length(tf))
  {
    vvars = c(vvars,paste(ltf[[i]]))
  }
  localdata  = data[,vvars]
  loessmod  = loess(formula,data=localdata)
  localdata[,1]= loessmod$residuals
  linmod = lm(formula,data=localdata)
  return(loessmod$fitted+linmod$fitted)
}

GenerateComponents = function(todecomp,maxorder)
{
  Nvar = ncol(todecomp)-1
  maxorder = min(maxorder,Nvar)
  allvarnames = names(todecomp)[2:(Nvar+1)]
  allcompnames = list()
  for(no in 1:maxorder)
  {
    Cno = combn(Nvar,no)
    for(k in 1:ncol(Cno))
    {
      C = Cno[,k]
      varnames = allvarnames[C]
      
      compname = paste(varnames,collapse="+")
      allcompnames = c(allcompnames,compname)
    }
  }
  return(allcompnames)
}

sample.decomp = function(todecomp,maxorder=1)
{
  Yname = names(todecomp)[1]
  Ncases = nrow(todecomp)
  result = data.frame(rep(mean(todecomp[[Yname]]),length(todecomp[[Yname]])))
  names(result)=c("{}")
  todecomp[[Yname]]= todecomp[[Yname]]-result[["{}"]]
  allcomps = GenerateComponents(todecomp,maxorder)
  for(k in 1:length(allcomps))
  {
    #formstring = paste("M~",allcomps[k])
    formstring = paste(Yname,"~",allcomps[k])
    newcomp  = linloess(as.formula(formstring),data=todecomp)
    result[[paste("{",allcomps[k],"}",sep="")]] = newcomp
    todecomp[[Yname]]= todecomp[[Yname]]-newcomp
  }
  return(result)
}

sample.vardecomp = function(todecomp,maxorder=1)
{
  dc = sample.decomp(todecomp,maxorder)
  return(apply(dc,MARGIN=2,FUN=var)[-1])
}
