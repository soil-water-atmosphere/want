// The hints are written in a dictionary, the key is the question number
dict = {};
dict[1] = "It can be handy to define the fluxes $\frac{c}{d}=a$ and boundary \begin{equation}a=b\end{equation} conditions in a separate variable. For example, 'BCleft=5'. In that way, they are easy to adapt and you prevent adding double fluxes.";
dict[2] = "You can use the nodes-vector to calculate the analytical solution for all x positions at the same time. Use substraction to look at the difference with the modelled heads. You can use the *dataframe.states()* function to get the modelled heads."; // returns the hint for the corresponding question_number in the corresponding paragraph field in the Rmd file.
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

var coll = document.getElementsByClassName("collapsible");
var i;

for (i = 0; i < coll.length; i++) {
  coll[i].addEventListener("click", function() {
    this.classList.toggle("active");
    var content = this.nextElementSibling;
    if (content.style.display === "block") {
      content.style.display = "none";
    } else {
      content.style.display = "block";
    }
  });
} 