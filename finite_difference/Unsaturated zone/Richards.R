rm(list = ls())
options(scipen=999)
setwd("/Users/jroebroek/Documents/")
rain_df = read.csv("neerslag_verdamping.txt")
rain_df = rain_df[2:nrow(rain_df),]
days    = seq(0,nrow(rain_df)-1,by=1)
rain    = rain_df$RH/10000
evap    = rain_df$EV24/10000
rain    = approxfun(days,rain-evap,method="linear")

# rain_f with variable flux (based on data)
rain_f  = function(lower,upper){return(integrate(rain,lower,upper, subdivisions=2000)[[1]])}

# rain_f for constant Neumann boundary
# rain_f = function(lower, upper){
#  dt = upper-lower
#  q  = 0.002 #value
#  return(q*dt)
# }

Richards = function(dt                 = 0.1,       # dt for stable situtaions
                    dt_c               = 0.005,     # dt for unstable situations
                    N                  = 20,        # number of nodes
                    Depth              = 0.5,       # soil depth (dz is derived from D and N)
                    #soil              = clay,      # this function might be built
                    plotting           = 0,         # "off" or interval of plotting
                                                    # 0 means plotting of all steps
                    report_stability   = FALSE,     # printing C and V
                    end.time           = 100,
                    rain_fun           = rain_f,    # only used when top_boundary="Neumann"
                                                    # this function needs to yield unit [m]
                                                    # it is divided by dt in the loop itself
                    maxiter            = 250,       # count if iterations before the best is
                                                    # chosen if system didn't converge
                    top_boundary       = "Dirichlet", # "Dirichlet"/"Neumann"
                    top_boundary_value = 0.4,       # only needed when top_boundary="Dirichlet"
                    BCswitch           = TRUE,      # if iterations doesn't yield any change,
                                                    # a Neumann BC may switch to a Dirichlet boundary
                                                    # when theta[1] is equal to theta_r
                    ret                = FALSE,     # return last calculated values
                    report             = FALSE,     # report progress on convergence etc.
                    slow_plotting      = TRUE){     # activate sys.sleep every time a plot is made
  # some kind of runoff function should be considered
  
  # parameters for a clay soil
  theta_r = 0.10
  theta_s = 0.46
  a       = 10^(-4)
  n       = 1.25
  m       = 0.20
  K_s     = 1.7*10^(-6)
  omega   = 0.5
  
  Se = function(theta){
    # function to calculate the Se parameter used in the van Genuchten equations
    return((theta-theta_r)/(theta_s-theta_r))
  }
  
  conductivity = function(theta){
    ### van Genuchten function for k
    # safety functions to don't let k out of the physical domain
    if(theta<=theta_r){return(0)}
    if(theta>=theta_s){return(K_s)}
    
    Se_c = Se(theta)
    A = Se_c^0.5
    B = 1-m*(1-Se_c^(1/m))^(m-1)
    C = (m-1)*(1-Se_c^(1/m))^m
    return(K_s*A*(B+C)^2)
  }
  
  diffusivity = function(theta){
    ### van Genuchten function for D
    # safety functions to don't let D out of the physical domain
    if(theta<=theta_r){return(0)}
    if(theta>=theta_s){return(diffusivity(theta=theta_s*0.98))}
    
    Se_c = Se(theta)
    return(((1-m)*K_s)/(a*m*(theta_s-theta_r))*Se_c^(1/2-1/m)*((1-Se_c^(1/m))^(-m)+(1-Se_c^(1/m))^m-2))
  }
  
  Courant = function(u,dt,dz){
    return(u*dt/dz)
  }
  
  Neumann = function(D,dt,dz){
    return((2*D*dt/dz^2))
  }
  
  # Parameters
  domain        = seq(0,Depth,length=N)
  dz            = domain[2]-domain[1]
  init.time     = 0
  init.theta    = rep(theta_r*1.15,N)
  
  theta_prev    = init.theta # (theta^n)
  
  # theta_new for the end result of the new time step (theta^(m+1))
  theta_new     = theta_prev 
  # theta_compare for comparing results between iterations (theta^m)
  theta_compare = theta_prev
  
  # initialisation plotting the flux 
  q     = 1
  q_new = 1
  
  # setting plotting parameters
  if(plotting=="off"){
    prevent_plotting = TRUE
    plotting = TRUE
    pl = 0
    dpl = 0
  } else {
    if(plotting < dt & plotting!=0){
      plotting = dt
    }
    pl       = plotting
    dpl      = plotting
    plotting = TRUE
    prevent_plotting = FALSE
  }
  
  top_boundary_original = top_boundary
  BCswitch_original = BCswitch
  
  step = 1
  time = init.time
  # small dt for the first timestep that hardly converges
  dt_origin  = dt
  if(top_boundary == "Neumann"){
    dt       = dt_c
  }
  dt_changed = TRUE
  
  # main loop
  while(time<end.time){
    # calculate the boundary for the new time-step. Store the old one for comparison
    q_old     = q_new
    q_new     = rain_fun(time,time+dt)
    # are these fluxes equal up to a certain tolerance
    eq        = isTRUE(all.equal(q_old,q_new, tolerance=0.000001))
    
    # routine to change to default dt to a lower value in case if big steps
    if(top_boundary != "Dirichlet" & 
       dt_changed==FALSE){
      # estimated theta with flux and previous thetas
      # if it falls close or over the boundaries, dt is reduced
      theta_est = theta_new[2]-dz*(1-q_new/dt/diffusivity((theta_new[1]+theta_new[2])/2))
      if((theta_new[1] < theta_r*1.01 | theta_new[1] > theta_s*0.97) & eq == FALSE){
        dt = dt_c
      } else if((theta_est < theta_r | theta_est    > theta_s)){
        dt = dt/4
        if(report){print(paste("estimated theta with current time_step: ",theta_est))}
      }
    }
    
    # error initialisation for first run
    error       = 1
    base_error  = 1
    # count the iterations
    count_inner = 0
    plotting    = TRUE
    
    # iterative loop to converge on the next step in the calculation
    while(error > 0.00001){
      # in the first step the system doesn't converge
      # take the best fit of the first maxiter trials seems a good approach
      if(maxiter>0){
        # if maxiter==0 it runs untill it converges
        count_inner = count_inner + 1
        if(error<base_error){
          # update best answer so far
          base_error = error
          base_theta_new = theta_new
        }
        if(count_inner>maxiter){
          # return best answer so far if no convergence in `maxiter` iterations
          if(step < 3 | # this might be smaller but it takes ages to come to this point otherwise
            dt <= dt_c){
            theta_new = base_theta_new
            if(report){print(paste("accepted maxiter with error = ", error, "; at step = ", step))}
          } else {
            theta_new = theta_prev
            dt   = dt/2
            if(dt<dt_c){dt = dt_c}
            dt_changed = TRUE
            plotting = FALSE
            if(report){print(paste("Doesn't converge iter>maxiter. New dt = ", dt, "; at step = ", step))}
            BCswitch = FALSE
          }
          break
        }
      }
      
      # theta compare is the stored previous iteration of theta for 
      # error comparison -> convergence
      theta_compare = theta_new
      M             = matrix(0,nrow=N,ncol=N)
      V             = matrix(0,nrow=N,ncol=1)
      
      # Dirichlet boundary condition top
      if(top_boundary == "Dirichlet"){
        M[1,1]     = 1
        V[1]       = top_boundary_value
      }
      
      # Neumann boundary condition top
      if(top_boundary == "Neumann"){
        M[1,1]     = -1
        M[1,2]     = 1
        V[1]       = dz*(1-(rain_fun(time,time+dt)/dt)/diffusivity((theta_new[1]+theta_new[2])/2))
        q          = rain_fun(time,time+dt)
      }
      
      # Dirichlet boundary condition bottom
      M[N,N]     = 1
      V[N]       = theta_r*1.15
      
      for(i in 2:(N-1)){
        ### filling up the matrix and vector
        # Diffusivity left and right (half a step) from the node -> up/down
        Dl = (diffusivity (theta_new[i-1]) + diffusivity (theta_new[i]))/2
        #Dl = diffusivity((theta_new[i-1] + theta_new[i])/2)
        
        Dr = (diffusivity (theta_new[i+1]) + diffusivity (theta_new[i]))/2
        #Dr = diffusivity((theta_new[i+1] + theta_new[i])/2)
        
        M[i,i-1] = -Dl/(dz^2)
        M[i,i  ] = (Dl+Dr)/(dz^2) + 1/dt
        M[i,i+1] = -Dr/(dz^2)
        V[i]     = (-conductivity(theta_new[i+1])+conductivity(theta_new[i-1]))/(2*dz) + theta_prev[i]/dt
      }
      
      theta_new = solve(M,V)
      
      # if NA shows up in the calculations, the time-step is halved and 
      # the calculations are repeated
      if((anyNA(theta_new)) & step!=1){
        theta_new = theta_prev
        dt   = dt/2
        dt_changed = TRUE
        plotting = FALSE
        if(report){print(paste("Doesn't converge. New dt = ", dt, "; at step = ", step))}
        BCswitch = FALSE
        break
      }
      
      if(step==1){
        # the first step hardly converges because of initial conditions
        # the error can thefore be quite large without disturbing the quality
        # too much
        error = sum(abs(theta_compare-theta_new))/1000000
      } else {
        error = sum(abs(theta_compare-theta_new))
      }
      
      # If things go out of the physical domain these lines bring them on 
      # the boundary
      theta_new[theta_new > theta_s] = theta_s
      theta_new[theta_new < theta_r] = theta_r
    
    } # end inner while loop  
    
    if(report &
       top_boundary!=top_boundary_original){
      print(paste("error with new BC = ", error))
    }
    
    ##################################################################
    # if a step yields exactly the same states, trigger a change in BC
    if(all(theta_prev == theta_new) &
       top_boundary   == "Neumann"  &
       BCswitch       == TRUE)
    {
      plotting = FALSE
      top_boundary = "Dirichlet"
      dt = dt_origin
      dt_changed = TRUE
      top_boundary_value = theta_r
      if(report){print("BC switched to Dirichlet type")}
    }
    BCswitch = BCswitch_original
    
    # plotting routine
    if(plotting           & 
       time      > pl     &
       !prevent_plotting) {
      
      pl = pl + dpl
      
      # break for plotting function
      if(slow_plotting){
        Sys.sleep(1/20)
      }
      
      # plotting functions
      plot(theta_new,
           -domain,
           type="l",
           main=paste("time",toString(format(round(time,2),nsmall = 2))),
           xlim=c(theta_r*0.9,theta_s*1.2))
      
      # plot theta_prev to compare
      lines(theta_prev,-domain,type="l",col="grey")
      
      if(all(theta_prev==theta_new) & report){print(paste("printing equal thetas",step))}
      
      abline(v=theta_r,col="lightgrey")
      abline(v=theta_s,col="lightgrey")
      # plot step
      text(0.52,-0.05, paste("iter = ", count_inner))
      # plot step
      text(0.52,-0.10, paste("step = ", step))
      # plot dt
      text(0.52, -0.15, paste("dt = ", dt))
      # plot flux
      if(top_boundary_original == "Neumann"){
        text(0.52,-0.20,paste("q = ", toString(format(round(q/dt,4),nsmall=4))))
      } else {
        text(0.52,-0.20,paste("q = ", NA))
      }
      # plot BC
      text(0.52,-0.25, top_boundary)
      # plot BC value
      if(top_boundary == "Dirichlet"){
        text(0.52, -0.30, paste("BC = ", toString(top_boundary_value)))
      }
    }
    
    if(plotting){
      # current step has been completed succesfully
      step = step + 1
      time = time + dt
      
      # resetting parameters
      theta_prev = theta_new
      dt = dt_origin
      dt_changed = FALSE
      top_boundary = top_boundary_original
      
      if(report){
        print("-------------------------------------------------------------")
        print(step)
      }
    }
    
    # Courant and Neumann numbers given when 
    if(report_stability){
      # print the stability numbers and check for boundary cases
      u = max(sapply(theta_new, conductivity))
      D = max(sapply(theta_new, diffusivity))
      C = Courant(u,dt,dz)
      V = Neumann(D,dt,dz)
      print(paste("C= ",toString(C),"; V= ", toString(V)))
    
    }
  }
  if(ret){return(theta_new)}
}

# even with maxiter=10000 not all steps converge, but they are still quite decent
# at the base of 250
# Richards(N=50, dt=0.2, dt_c = 0.01, top_boundary = "Neumann", report=TRUE)

# Richards(N=50, dt=0.2, dt_c = 0.01, top_boundary = "Neumann", BCswitch = FALSE)

# simulating a water front going through a dry soil
# works even at a high dt
# Richards(N = 50, dt = 5, report=TRUE)

# this needs a lot of time-step adapting but runs quite fast!
# too high doesn't lead to any faster run-time
# Richards(N=50, dt=1, dt_c = 0.01, top_boundary = "Neumann", report_stability = FALSE, plotting = 0)
