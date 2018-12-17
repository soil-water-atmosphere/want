// The hints are written in a dictionary, the key is the question number
dict = {};
dict[41] = "The domain is defined by coordinates of two dimensions. Therefore, the domain is now defined by a 2 dimensional matrix. Type '?matrix()' in your console for more information.";
dict[42] = "Remember that a Neumann natural boundary condition is assigned to unspecified boundaries.";
dict[45] = "Note that you can suppress graphing of the node generation by setting 'verbose=False'.";
dict[413] = "Change both the domain values of 'xtopl' and 'ytopl' before interpolating the two dimensional model domain of TwoDmodel with the new transmissivities.";
dict[417] = "add: welldist = func.dist.to.point(c(1000,1000),200,25,100) and set.discretisation(TwoDModelWell, nodes=list(type='geomgen', optdist=welldist, verbose=FALSE), method='FE') on separate lines to decrease the nodal distance around the location of the well.";
dict[419] = "Check the documentation of the func.dist.to.line() function in the FVFE2D package.";
dict[420] = "Define domain range: plot(c(0,10), c(0,2), xlab='x', ylab='y', col='white') Plot polygon: polygon(domain, lwd=3, col=rgb(0.7,0.7,0.7,0.3)).";
dict[422] = "Remember the function plot.BC.id().";
dict[425] = "The pressure function has 3 arguments: x, y and state. Its body contains one line: a return statement.";

// returns the hint for the corresponding question_number in the corresponding paragraph field in the Rmd file.
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