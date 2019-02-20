

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
  d == 0;
  v == 0;
}

finally {
  d >= 30;
  v == 0;
}
