// The hints are written in a dictionary, the key is the question number
dict = {};
dict[1] = "You can use the nodes-vector to calculate the analytical solution for all x positions at the same time. Use substraction to look at the difference with the modelled heads. You can use the *dataframe.states()* function to get the modelled heads.";
dict[2] = "(To be added: Explanation on Monin-Obukhov length/atmospheric stratification)";

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