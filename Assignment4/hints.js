// The hints are written in a dictionary, the key is the question number
dict = {};
dict[41] = "The domain is defined by coordinates of two dimensions. Therefore, the domain is now defined by a 2 dimensional matrix. Type '?matrix()' in your console for more information.";
dict[42] = "Remember that a Neumann natural boundary condition is assigned to unspecified boundaries.";



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