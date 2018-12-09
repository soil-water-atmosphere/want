rm(list=ls())
library(plotly)

### SOLVING THE ADVECTION DIFFUSION EQUATION WITH CN,EXPLICIT OR IMPLICIT SCHEME
# INPUT
solve_ad = function(domain=c(0,100),
                    endtime=20,
                    dx=1,
                    dt=1,
                    u=1,
                    D=1,
                    bl=0,
                    br=0,
                    omega=1,
                    initfun=approxfun(c(0,45,50,55,100),c(0,0,1,0,0),rule=2)
                    ){
  # omega = 1   , explicit
  # omega = 0   , implicit
  # omega = 0.5 , CN
  
  # CALCULATION PREPARATION
  x           = seq(domain[1],domain[2],by=dx)
  N           = length(x)
  init.state  = initfun(x)
  
  # MATRIX CONSTRUCTION
  M = matrix(0,nrow=N,ncol=N)
  
  # for now a dirichlet boundary is used for the solution
  M[1,1] = 1
  M[N,N] = 1
  
  for(i in 2:(N-1)){
    M[i,i-1] = (omega-1) * ( u/(2*dx) +   D/dx^2 )
    M[i,i  ] = (omega-1) * (          - 2*D/dx^2 ) + 1/dt
    M[i,i+1] = (omega-1) * (-u/(2*dx) +   D/dx^2 )
  }
  
  Minv = solve(M) # this can be done in when the final calculation is done but this means that the inverting of matrix needs to be done over and over again
  
  # SET UP ANALITICAL SOLUTION
  a.x            = seq(domain[1],domain[2],length=1000)
  sd0            = (domain[2]-domain[1])/200
  sampleweights  = initfun(a.x)
  a.init.state   = rep(0,length(a.x))  
  state.range    = range(init.state)
  
  for(i in 1:length(a.x))
  {
    a.init.state = a.init.state + 
      sampleweights[i]*dnorm(a.x,mean=a.x[i],sd=sd0)
  }
  
  sampleweights = sampleweights/max(a.init.state)*state.range[2]
  
  # STORE ANALITICAL RESULTS
  a.N           = length(a.x)
  Nsteps        = endtime/dt+1
  a.steps       = rep(1:Nsteps,each=a.N)
  a.values      = rep(0,length=length(a.steps))
  a.domain      = rep(a.x,length=length(a.steps))
  
  solution = data.frame("step" = a.steps, "x"= a.domain, "value" = a.values, "tvalue" = a.values)
  
  solution[solution$step == 1,]$tvalue = initfun(a.x)
  solution[solution$step == 1,]$value  = initfun(a.x)
  
  step       = 1
  new.state  = init.state
  time       = 0
  
  while(time<endtime){
    # analytical solution
    a.state = rep(0,length(a.x))
    for(i in 1:length(a.x))
    {
      a.state = a.state+ 
        sampleweights[i]*dnorm(a.x-u*time,mean=a.x[i],sd=sqrt(sd0^2+2*time*D))
    }
    
    # approximated solution
    prev.state = new.state
    
    V    = c()
    V[1] = bl
    
    for(i in 2:(N-1))
    {
      V[i]= prev.state[i]/dt + omega*(-u*(prev.state[i+1]-prev.state[i-1])/(2*dx)
                                      +D*(prev.state[i+1]-2*prev.state[i]+prev.state[i-1])/dx^2)
    }
    
    V[N] = br
    new.state = Minv %*% V
    
    step = step + 1
    time = time + dt
    
    newstate.fun = approxfun(x,new.state,rule=2)
    solution[solution$step == step,]$tvalue = a.state
    solution[solution$step == step,]$value  = newstate.fun(a.x)
    #Sys.sleep(1/8)
  }
  
  # plotting
  state.length = state.range[2]-state.range[1]
  plotlim      = c(state.range[1]-0.2*state.length,state.range[2]+0.2*state.length)
  
  # for(i in 1:25){
  #   plot(a.x,solution[solution$step == i,]$value,ylim=plotlim,type="l")
  #   lines(a.x,solution[solution$step == i,]$tvalue, col="grey",lwd=2)
  #   Sys.sleep(1/8)
  # }
  
  plot_ly(
    solution,
    x          = ~x,
    y          = ~tvalue,
    frame      = ~step,
    type       = "scatter",
    mode       = "lines",
    #showlegend = T,
    line = list(
      color = 'lightgrey',
      width = 2
    )
  ) %>% 
    add_trace(
      y          = ~value,
      frame      = ~step,
      type       = "scatter",
      mode       = "lines",
      line = list(
        color = 'blue',
        width = 1
      )
    ) %>%
    animation_opts(
      frame      = 200,
      transition = 0,
      easing     = "linear",
      redraw     = FALSE,
      mode       = "immediate"
    ) %>% 
    layout(
      xaxis      = list(range=a.x),
      yaxis      = list(range=plotlim)
    ) %>% 
    add_annotations(
      xref  = "paper",
      yref  = "paper",
      x     = 1,
      y     = 1,
      text  = paste("D = ", as.character(D) , "\n", "u = " , as.character(u)),
      showarrow = F,
      align = "left"
    ) %>% 
    add_annotations(
      xref  = "paper",
      yref  = "paper",
      x     = 0.05,
      y     = 1,
      align = "left",
      text  = (paste("omega = ", as.character(omega) , "\n", "dx = " , as.character(dx), "\n", "dt = " , as.character(dt))),
      showarrow = F
    )
}

solve_ad(omega=1,dt=0.8)
