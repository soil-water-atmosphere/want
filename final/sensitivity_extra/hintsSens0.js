// The hints are written in a dictionary, the key is the question number
dict = {};

dict[1] = "During the lectures, we only evaluated the influence of different parameters on a single value (in the example discused in the lectures, this was the maximum groundwater level). In this exercise, you suddenly deal with a vector of 11 values. What we need in order to determine the sensitivity, is a way of expressing changes in the vector y in a single value, related to the observations. The sums of squares could be a good option.";

dict[2] = "The sums of squares can be obtained as follows: SS = sum(yobserved - ymodel)^2";

dict[3] = "Calculating a response surface requires sampling the parameters between the provided ranges. With a stepsize of 0.1, we have 21 different values for a, and 31 different values for b. That leads to a total of 21*31 is 651 different parameter sets. Decreasing the stepsize, or increasing the number of parameters, also drastically increases the total number of parameter sets that have to be evaluated. [check: the curse of dimensionality]";
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