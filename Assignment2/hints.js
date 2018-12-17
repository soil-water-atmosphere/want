// The hints are written in a dictionary, the key is the question number
dict = {};
dict[212] = "The model resembles the model made in the previous assignment.";
dict[213] = "Check the documentation of the dataframe.* functions.";
dict[214] = "Check the documentation of dataframe.balance().";
dict[216] = "Read the documentation of the copy.model() function.";
dict[218] = "It might seem a bit odd to use the function oldstate(x) here since it is not defined yet. However, as long as we only define the storageflux function and not actually run it, we are allowed to use the undefined the oldstate(x) function. As long as we make sure that the oldstate(x) function will be defined and run before the storageflux calculation is run, no exception will be raised.";
dict[2120] = "Before the loop construct is entered, you can basically copy the previous code that describes the two time increments. Do not forget to assign the proper state values to the oldstate function before entering the while loop.";
dict[2123] = "Create a function that simulates the potential temperature profile in time taking as its only argument the time step for which should be solved. Return a data container in which the profiles are stored. Run this function in a for loop for every time step and plot the result for comparison.";
dict[222] = "Think about the different sections in the model setup. First you define your system properties, then the mathematical part and finally the numerical part which makes the model ready to solve.";
dict[224] = "Several useful data frames are described in the documentation.";
dict[225] = "In order to add a graph to a figure, first the figure is created with the plot() function, thereafter lines() or points() can be used to add data.";
dict[228] = "One should be able to initialize the states of the transient model by a combination of the functions do.initialize() and state.fun().";
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