// The hints are written in a dictionary, the key is the question number
dict = {};
dict[1] = "y = function(x){....}";
dict[2] = "If we approximate non-linear systems with linear ones, we can note that as follows: $f(x) \approx \tilde{f}(x)$";
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