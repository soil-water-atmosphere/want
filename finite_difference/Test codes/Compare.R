library(shiny)
compare = function(dt,runge_kutta,midpoint,explicit,implicit){
  if(!any(runge_kutta,midpoint,explicit,implicit)){
    plot.new()
    return()
  }
  
  # Other model parameters
  begin.time = 0     # usually this variable is set to 0
  end.time   = 50    # end time of the simulation
  init.state = 3     # state of the system at the beginning of the simulation
  alpha      = 1     # decay constant
  A          = 4     # surface of the bucket
  l          = c()   # empty list for legend entries
  lines_l    = c()   # empty list for legend lines
  
  f = function(time,state)
  {
    return(((sin(time)+1)-alpha*state)/A)
  }
  
  ## Background plot (based on Runge-Kutta)
  
  sys.fun = function(state)
  {
    k1       = dt*f(time       , state         )
    k2       = dt*f(time+0.5*dt, state + 0.5*k1)
    k3       = dt*f(time+0.5*dt, state + 0.5*k2)
    k4       = dt*f(time+    dt, state +     k3)
    newstate = state + (k1 + 2*k2 + 2*k3 + k4)/6
    return(newstate)
  }
  
  time = begin.time
  result.state= c(init.state)
  result.time =c(time)
  current.state = init.state
  dt_temp = dt
  dt = 0.1
  while(time < end.time)
  {
    current.state = sys.fun(current.state)
    result.state = c(result.state,current.state)
    time = time + dt
    result.time = c(result.time,time)
  }
  
  plot(result.time,result.state,type='l',col='darkgrey',lwd=3, xlab="time",ylab="state")
  
  # resetting dt
  dt = dt_temp   
  
  ##########SETTINGS#############
  ########Runge Kutta############
  time = begin.time  
  result.state= c(init.state)  
  result.time =c(time)  
  current.state = init.state  
  while(time < end.time)  
  {  
    current.state = sys.fun(current.state)  
    result.state = c(result.state,current.state)  
    time = time + dt  
    result.time = c(result.time,time)  
  }  
  if(runge_kutta){
    lines(result.time,result.state,type='o',col='black')
    l       = c(l,'Runge Kutta')
    lines_l = c(lines_l,"black")
  }
  
  ###########Midpoint##################
  sys.fun = function(state)
  {
    s.aux    = state + dt/2*f(time,state)
    newstate = s.aux + dt/2*f(time+dt/2,s.aux)
    return(newstate)
  }
  
  time = begin.time
  result.state= c(init.state)
  result.time = c(time)
  current.state = init.state
  while(time < end.time)
  {
    current.state = sys.fun(current.state)
    result.state = c(result.state,current.state)
    time = time + dt
    result.time = c(result.time,time)
  }
  if(midpoint){
    lines(result.time,result.state,type='o',col='red')
    l       = c(l,'Midpoint')
    lines_l = c(lines_l,"red")
  }
  
  ###########Implicit##################
  sys.fun = function(state)
  {
    newstate = 1/(1+alpha*dt/A)*(state + dt/A*(sin(time+dt)+1))
    return(newstate)
  }
  
  time = begin.time
  result.state= c(init.state)
  result.time =c(time)
  current.state = init.state
  while(time < end.time)
  {
    current.state = sys.fun(current.state)
    result.state = c(result.state,current.state)
    time = time + dt
    result.time = c(result.time,time)
  }
  if(implicit){
    lines(result.time,result.state,type='o',col='blue')
    l       = c(l,'Implicit')
    lines_l = c(lines_l,"blue")
  }
  
  ###########Explicit##################
  sys.fun = function(state)
  {
    newstate = state + dt/A*((sin(time)+1)-alpha*state)
    return(newstate)
  }
  
  time = begin.time
  result.state= c(init.state)
  result.time = c(time)
  current.state = init.state
  while(time < end.time)
  {
    current.state = sys.fun(current.state)
    result.state = c(result.state,current.state)
    time = time + dt
    result.time = c(result.time,time)
  }
  if(explicit){
    lines(result.time,result.state,type='o',col='green')
    l       = c(l,'Explicit')
    lines_l = c(lines_l,"green")
  }
  
  lty_l = rep(1  ,length(lines_l))
  lwd_l = rep(2.5,length(lines_l))
  
  legend('topright', l, lty=lty_l, lwd=lwd_l, col=lines_l)
  title(main=paste("Comparing quality integration schemes,",' dt :' ,dt))
  
  grid()
}



#########################################
ui <- fluidPage(
  
  # App title ----
  titlePanel("Compare integration schemes"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      sliderInput("dt", label = "dt:",min = 0.2, max = 20, value = 1, step = 0.2),
      checkboxInput("runge_kutta", "Runge-Kutta", FALSE),
      checkboxInput("midpoint"   , "Moidpoint"  , FALSE),
      checkboxInput("explicit"   , "Explicit"   , FALSE),
      checkboxInput("implicit"   , "Implicit"   , FALSE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("p")
      
    )
  )
)

server <- function(input, output) {
  output$p = renderPlot({compare(input$dt,input$runge_kutta,input$midpoint,input$explicit,input$implicit)})
}

shinyApp(ui, server)



