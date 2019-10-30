// The hints are written in a dictionary, the key is the question number
dict = {};

dict[1] = ` # Domain <BR/>
 domain = c(0,500)`;

dict[2] = `# Internal flux <BR/> kDl = 55 <BR/> kDr = 35 <BR/> kD.fun = approxfun(domain,c(kDl,kDr)) <BR/> #gives a (linear) function for the transmissivity # caution: every time you change kDl or kDr you have to rerun this function <BR/>
internal.flux = function(x,state,gradstate) <BR/>
  { <BR/>
  return(-kD.fun(x)*gradstate) <BR/>
  }`;
  
dict[3] = `# Define the model <BR/>
GWmodel = newFLOW1D(domain,internal.flux) <BR/>
set.name(GWmodel,"base run") <BR/><BR/>
# External fluxes <BR/>
P = 0.001 <BR/>
add.spatialflux(GWmodel,"P","recharge") <BR/>
Qwell = -0.35 <BR/>
add.pointflux(GWmodel,at = 275, value= 'Qwell',name = 'well')`;

dict[4] = `# Set boundary conditions <BR/>
hl = 1 <BR/>
set.BC.fixedstate(GWmodel,"left","hl")`;

dict[5] = `# numerical setup <BR/>
nodes = seq(from=domain[1],to=domain[2],length=50)<BR/> 
set.discretisation(GWmodel,nodes,"FV")`;

dict[6] = `# run the model <BR/>
control = solve.steps(GWmodel) <BR/>
plot(GWmodel,fluxplot=TRUE)`;

dict[7] = "Sensitivity is change in output as a result of change in input. The change in output is the difference between MRESULT (the last run) van M_base (the base run). The change in input is epsilon; this is the change in the parameter value. Sensitivity can then be calculated as:  (MRESULT()-M_base)/eps";

dict[8] = `#Add epsilon-value to base-value of parameter P, keep all other parameters at base-value<BR/>
kDl=base$kDl+eps<BR/>
kD.fun = approxfun(domain,c(kDl,kDr)) # the kD.fun has to be recreated whenever the kD values are changed<BR/><BR/>
# Rerun the model<BR/>
control = solve.steps(GWmodel)<BR/><BR/>
# Set parameter P back to base value (for next parameter)<BR/>
kDl=base$kDl<BR/>
kD.fun = approxfun(domain,c(kDl,kDr)) <BR/><BR/>
#Calculate the sensitivity of MRESULT to parameter P <BR/>
dMdkDl  = (MRESULT()-M_base)/eps<BR/>
print(paste("dM/dkDl=",dMdkDl))<BR/><BR/>
# Calculate the sensitivity of the model states to parameter P<BR/>
dstatedkDl = (dataframe.states(GWmodel)$state-state_base)/eps`;

dict[9] = `# Calculate the partial variances of all parameters using the scale of variation<BR/>
varM.kDl   = (scale$kDl*dMdkDl)^2<BR/>
# .. add all parameters.. <BR/><BR/>
# Calculate the total model variance<BR/>
varMtot = varM.kDl+varM.kDr+varM.P+varM.Qwell+varM.hl<BR/><BR/>
# Determine the relative contribution of each parameter <BR/>
relvarkDl = varM.kDl/varMtot`;

dict[10] = `# Calculate the partial variances of all parameters using the scale of variation<BR/>
varState.kDl = (scale$kDl*dstatedkDl)^2<BR/>
# .. add all parameters.. <BR/><BR/>
# Calculate the total model variance <BR/>
varStateTot = varState.kDl+varState.kDr+varState.P+varState.Qwell+varState.hl <BR/><BR/>
# Plot the results over the nodes, to see changes in sensitivity over the domain <BR/>
plot(nodes,sqrt(varStateTot),type="l",col="blue",lwd=3,<BR/>
     ylab="standard deviation [m]",xlab="x",main="Total scale of variation of groundwater levels")<BR/>
grid(col="black")`; 


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