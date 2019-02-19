
var x: Real;

subject_to {
  x*x - 25;
}

expected {
  x = 5; // -5 would also be valid
}