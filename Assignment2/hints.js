// The hints are written in a dictionary, the key is the question number
dict = {};
dict[211] = "The model resembles the model made in the previous assignment.";
dict[212] = "Check the arguments of the plot function of the FVFE1D package to make both plots in one line of code.";
dict[213] = "Check the documentation of dataframe.balance().";
dict[215] = "Read the documentation of the copy.model() function.";
dict[2113] = "Check the arguments of the plot function of the FVFE1D package to make both plots in one line of code.";
dict[2119] = "Before the loop construct is entered, you can basically copy the previous code that describes the two time increments. Do not forget to assign the proper state values to the oldstate function before entering the while loop.";
dict[222] = "Think about the different sections in the model setup. First you define your system properties, then the mathematical part and finally the numerical part which makes the model ready to solve.";
dict[224] = "Several useful data frames are described in the documentation.";
dict[225] = "In order to add a graph to a figure, first the figure is created with the plot() function, thereafter lines() or points() can be used to add data.";
dict[2211] = "What are the obligatory arguments in the system flux function? Which of these can be used for the saturated thickness?";
dict[2213] = "What is the time resolution of your recharge data?";


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