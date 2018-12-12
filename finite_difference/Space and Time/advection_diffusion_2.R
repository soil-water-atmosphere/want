rm(list=ls())
library(plotly)
library(shiny)

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
                    initfun= function(x){
                      exp(-(x-domain[2]/2)^2/2^2)
                    },
                    delay = 200
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
  a.x            = seq(domain[1],domain[2],length=500)
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
  steps         = seq(0,endtime,by=dt)
  Nsteps        = length(steps)-1
  a.steps       = rep(1:Nsteps,each=a.N)
  a.values      = rep(0,length=length(a.steps))
  a.domain      = rep(a.x,length=length(a.steps))
  
  solution = data.frame("step" = a.steps, "t" = a.steps*dt ,"x"= a.domain, "value" = a.values, "tvalue" = a.values)
  
  solution[solution$step == 1,]$tvalue = initfun(a.x)
  solution[solution$step == 1,]$value  = initfun(a.x)
  
  step       = 1
  new.state  = init.state
  time       = 0
  
  while(step<(Nsteps)){
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
  }
  
  # plotting
  state.length = state.range[2]-state.range[1]
  plotlim      = c(state.range[1]-0.2*state.length,state.range[2]+0.2*state.length)
  
  plot_ly(
    solution,
    x          = ~x,
    y          = ~tvalue,
    frame      = ~t,
    type       = "scatter",
    mode       = "lines",
    name       = "True solution",  
    line = list(
      color = 'lightgrey',
      width = 2
    )
  ) %>% 
    add_trace(
      y          = ~value,
      frame      = ~t,
      type       = "scatter",
      mode       = "lines",
      line = list(
        color = 'blue',
        width = 1
      ),
      name = "Approximation"
    ) %>%
    animation_opts(
      frame      = delay,
      transition = 0,
      easing     = "linear",
      redraw     = FALSE,
      mode       = "immediate"
    ) %>% 
    layout(
      xaxis      = list( title = "space",
                         range=a.x),
      yaxis      = list( title = "state",
                         range=plotlim)
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
    ) %>%
    animation_slider(
      currentvalue = list(prefix = "t = ", font = list(color="darkblue"))
    )
}

#################SHINY###################
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    h4 {
                    color: darkblue;
                    }
                    "))
    ),
  # App title ----
  titlePanel("Advection Diffusion Equation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      h4("Equation"),
      sliderInput("u", label = "u:",min = 0, max = 10, value = 1, step = 0.1),
      sliderInput("D", label = "D:",min = 0, max = 10, value = 1, step = 0.1),
      hr(),
      h4("Approximation settings"),
      sliderInput("dt", label = "dt:",min = 0.1, max = 10, value = 1, step = 0.1),
      sliderInput("dx", label = "dx:",min = 0.1, max = 10, value = 1, step = 0.1),
      div("Omega = 1 (Explicit)"         , style = "font-style:italic; font-size:7pt"),
      div("Omega = 0 (Implicit)"         , style = "font-style:italic; font-size:7pt"),
      div("Omega = 0.5 (Crank-Nicolson)" , style = "font-style:italic; font-size:7pt"),
      sliderInput("omega", "Time-integration scheme (omega):", min=0,max=1,value=0,step=0.1),
      hr(),
      h4("Plotting settings"),
      sliderInput("delay", label = "Delay between frames:",min = 0, max = 5000, value = 200, step = 50),
      sliderInput("endtime", label = "Runtime:",min = 1, max = 100, value = 20, step = 1),
      hr(),
      submitButton(text = "Apply Changes", icon = NULL, width = NULL)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot of the requested variable against mpg ----
      plotlyOutput("p")
      
    )
  )
)

server <- function(input, output) {
  output$p  = renderPlotly({solve_ad(dt      = input$dt,
                                     dx      = input$dx,
                                     omega   = input$omega,
                                     u       = input$u,
                                     D       = input$D,
                                     delay   = input$delay,
                                     endtime = input$endtime)})
}

shinyApp(ui, server)
