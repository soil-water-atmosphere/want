// The hints are written in a dictionary, the key is the question number
dict = {};
dict[211] = "The model resembles the model made in the previous assignment.";
dict[212] = "Check the arguments of the plot function of the FVFE1D package to make both plots in one line of code.";
dict[213] = "Check the documentation of dataframe.balance().";


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