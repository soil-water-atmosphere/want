rm(list=ls())
error = function(taylor){
  # Taylor series parameter
  #taylor     = 15
  
  # System function
  sys.fun = function(state)
  {
    newstate = state*(1-alpha*dt)
    return(newstate)
  }
  
  # Initial parameter values
  begin.time = 0     # usually this variable is set to 0
  end.time   = 50    # end time of the simulation (50)
  dt         = 5     # delta t; time discretisation (0.25)
  init.state = 3     # state of the system at the beginning of the simulation (3)
  alpha      = 0.25  # decay constant (0.25)
  
  # Simulation initialisation
  time = begin.time
  result.state= c(init.state)
  result.time = c(time)
  current.state = init.state
  
  # Simulation
  while(time < end.time)
  {
    current.state = sys.fun(current.state)
    result.state = c(result.state,current.state)
    time = time + dt
    result.time = c(result.time,time)
  }
  
  # Taylor series approximation
  derivative.list = c(paste(toString(init.state),"*DT^0"))
  f = expression(init.state*exp(-alpha*t))
  t = 0
  if(taylor>1){
    for(i in 1:(taylor-1)){
      f = D(f,'t')
      derivative.list = c(derivative.list,paste(toString(eval(f)),"*DT^",toString(i),"/",toString(factorial(i))))
      print(f)
    }
  }
  g = parse(text = substitute(expression(f),list(f=paste(derivative.list,collapse = "+"))))
  
  
  DT = seq(begin.time,end.time,by=0.1)
  p  = eval(g)
  
  # Plotting
  plot(result.time,result.state,type='o', xlab="time",ylab="state")
  time.sequence = seq(begin.time,end.time,by=0.1)
  analyt.state=init.state * exp(-time.sequence*alpha)
  lines(time.sequence,analyt.state,col='blue',lwd=2)
  lines(DT,p, lwd=2, col="green")
  
  DT = dt
  correct.state = init.state*exp(-dt*alpha)
  error = round(abs(correct.state-eval(g)),3)
  points(dt,eval(g),col="green",pch=19)
  points(dt,correct.state,pch=19)
  segments(dt,correct.state,dt,eval(g),col="red",lwd=2)
  title(main=paste('Numerical and analytical solution compared. Error: ',error))
  
  grid()
}

#########################################
ui <- fluidPage(
  
  # App title ----
  titlePanel("Calculate the error"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      numericInput("taylor", label  = "Terms of the taylor series:" , value=1, min = 1, max = 40, step = 1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("p")
      
      
      
    )
  )
)

server <- function(input, output) {
  output$p = renderPlot({error(input$taylor)})
}

shinyApp(ui, server)




