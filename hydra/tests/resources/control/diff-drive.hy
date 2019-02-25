
constant L: Real = 10;

fluent x: Real;
fluent y: Real;
fluent theta: Real;

control v: Real;
control s: Real;

dynamics {
  dot(x) = v * cos(theta);
  dot(y) = v * sin(theta);
  dot(theta) = v * tan(s) / L;
}

subject_to {
  v <= 10;           // v in [0, 10]
  v >= 0;
  abs(s) <= PI /2;   // s in [-PI/2, PI/2]
}

initially {
  x == 0;
  y == 0;
  theta == 0;
  v == 0;
}

finally {
  x == 10;
  y == 10;
  theta == PI;
  v == 0;
}