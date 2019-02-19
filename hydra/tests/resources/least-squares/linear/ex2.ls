
constant Pi: Real = 3.14159;

var x: Real;
var y: Real;
var z: Real;


subject_to {
  x + y - 10;
  y - 5;
  z - Pi;
}

expected {
  x = 5;
  y = 5;
  z = 3.14159;
}