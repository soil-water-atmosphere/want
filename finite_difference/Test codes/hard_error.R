rm(list=ls())
# Initial parameter values
begin.time = 0     # usually this variable is set to 0
end.time   = 50    # end time of the simulation (50)
dt         = 5     # delta t; time discretisation (0.25)
init.state = 3     # state of the system at the beginning of the simulation (3)
alpha      = 0.25  # decay constant (0.25)

# System function
sys.fun = function(state)
{
  newstate = state*(1-alpha*dt)
  return(newstate)
}

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

f = expression((-0.75 + 0.1875 * DT^(1)/2) * DT + 3)   # constants obtained by integration and evaluation (function D() and eval())

DT = seq(begin.time,end.time,by=0.1)  # list of evaluation times
p  = eval(f)                          # list of results of the implemented funtion

# Plotting
plot(result.time,result.state,type='o', xlab="time",ylab="state")
time.sequence = seq(begin.time,end.time,by=0.1)
analyt.state=init.state * exp(-time.sequence*alpha)
lines(time.sequence,analyt.state,col='blue',lwd=2)
lines(DT,p, lwd=2, col="green")

DT = dt
correct.state = init.state*exp(-dt*alpha)
error = round(abs(correct.state-eval(f)),3)
points(dt,eval(f),col="green",pch=19)
points(dt,correct.state,pch=19)
segments(dt,correct.state,dt,eval(f),col="red",lwd=2)
title(main=paste('Numerical and analytical solution compared. Error: ',error))

grid()