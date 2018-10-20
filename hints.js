// The hints are written in a dictionary, the key is the question number
dict = {};
dict[1] = "The concepts field capacity (pF 2) and wilting point (pF 4.2) are important here. When the water content of the soil is above the field capacity (pF<2) water will drain by gravity. The water in the soil below field capacity is the water that the soil can retain and supply to vegetation. However, when the soil water content is below wilting point (pF<4.2) the water is attached so strongly to the soil particles that plants cannot extract it any more. Therefore, the amount of water between field capacity and wilting point is considered to be the plant available water."
dict[2] = "You can solve this question in two ways. 1: Ponding occurs when the infiltration capacity is exceeded, that is, the downwards flow (q) is higher than Ksat. From this you can derive the maximum q per soil type (without using the code chunk). Would groundwater depth be important here? 2: Ponding means that the soil gets saturated to the surface. You can use the chunk to try out how much recharge the model can handle without getting fully saturated.";
dict[3] = "When you extract water from the top, the model will try to balance this with an upward water flux from the lower boundary. However, when the outgoing flow at the top is very high, drawing enough flow from the bottom will need extremely low pressures at the surface, that may be below the 'realistic minimum' (<-1E-5) that we have set. The highest evaporation flux at the top that still gives a solution can be seen as the maximum capillary rise.";
dict[4] = "You can use the theta-profile and the size of the control volumes.";

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