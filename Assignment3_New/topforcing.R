
evapfun = function(t)
{
  return(0.3*max(0.3+sin(t*(2*pi)-0.5),0))
}


rainfun = function(t)
{
  result = -1
  result = result+sin(6.28*t/5+0.1)
  result = result+sin(8.17*t/5+0.4)
  result = result+sin(15.71*t/5+0.2)
  result = result+sin(32.04*t/5+0.1)
  return(max(result,0))
}


# tseq = seq(0,100,by=1/24)
# rainseq = c()
# for(i in 1:length(tseq)) rainseq[i] =rainfun(tseq[i])
# plot(tseq,rainseq,type="h")
