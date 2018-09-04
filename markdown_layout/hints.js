// The hints are written in a dictionary, the key is the question number
dict = {};
dict[1] = "Hint for the first question";
dict[2] = "Hint for the second question"
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