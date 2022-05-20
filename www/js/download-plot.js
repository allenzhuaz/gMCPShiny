function prepHref(linkElement) {
  var myDiv = document.getElementById("thePlot");
  var myImage = myDiv.children[0];
  linkElement.href = myImage.src;
}
