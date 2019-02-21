

fluent d: Real;
fluent v: Real;
control a: Real;

dynamics {
  dot(d) = v;
  dot(v) = a;
}

subject_to {
  v <= 100 || a <= 0;
  abs(a) <= 1;
}

initially {
  d == 5;
  v == 3;
}

finally {
  d >= 30;
  v == 4;
}
