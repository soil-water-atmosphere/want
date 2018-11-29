// The hints are written in a dictionary, the key is the question number
dict = {};
dict[1] = "y <- function(x){....}";
dict[2] = "We approximate non-linear systems with linear ones, nonlin(x)~lin(x), by determining the slope around an estimate of x: lin(x) = lin(estimate)+ ((lin(estimate+epsilon)-lin(estimate))/epsilon)*(x-estimate)";
dict[3] = "";
// dict[3] = extend with new hints by extending this dictionary


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