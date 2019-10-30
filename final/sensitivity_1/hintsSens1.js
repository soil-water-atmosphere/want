// The hints are written in a dictionary, the key is the question number
dict = {};

dict[1] = "
## Domain
# domain = c(0,500) 

## Internal flux
#kDl = 55
#kDr = 35
#kD.fun = approxfun(domain,c(kDl,kDr)) #gives a (linear) function for the transmissivity
## caution: every time you change kDl or kDr you have to rerun this function
#internal.flux = function(x,state,gradstate) 
#  {
#  return(-kD.fun(x)*gradstate)
#  }

## Define the model
# GWmodel = newFLOW1D(domain,internal.flux) 
# set.name(GWmodel,"base run")

## External fluxes
# P = 0.001
# add.spatialflux(GWmodel,"P","recharge")
# Qwell = -0.35
# add.pointflux(GWmodel,at = 275, value= 'Qwell',name = 'well')

## Set boundary conditions
# hl = 1
# set.BC.fixedstate(GWmodel,"left","hl") 

## numerical setup
# nodes = seq(from=domain[1],to=domain[2],length=50) 
# set.discretisation(GWmodel,nodes,"FV")

## run the model
# control = solve.steps(GWmodel)
#plot(GWmodel,fluxplot=TRUE)";

dict[2] = "The sums of squares can be obtained as follows: SS = sum(yobserved - ymodel)^2";

dict[3] = "Calculating a response surface requires sampling the parameters between the provided ranges. With a stepsize of 0.1, we have 21 different values for a, and 31 different values for b. That leads to a total of 21*31 is 651 different parameter sets. Decreasing the stepsize, or increasing the number of parameters, also drastically increases the total number of parameter sets that have to be evaluated. [check: the curse of dimensionality]";
function showHint(question_number) {
	if (document.getElementById("Q"+question_number.toString()).innerHTML == " ") {
		var Qnum = question_number;
		document.getElementById("Q"+question_number.toString()).innerHTML = dict[Qnum];
	}
	else {
		var empty = " ";
		document.getElementById("Q"+question_number.toString()).innerHTML = empty;
	}
}