var x: Real;
var y: Real;

subject_to {
  x * y - 100;
  y - 1;
}

expected {
  y = 1;
  x = 100;
}