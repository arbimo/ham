
constant M_min: Real = 5000;
constant d_margin: Real = 1;
constant v_margin: Real = 1;
constant g: Real = 9.89;
constant ISP: Real = 311;
constant max_q: Real = 50;

constant d_final: Real = 300;

fluent M: Real;
fluent d: Real;
fluent v: Real;

control q: Real;

dynamics {
  dot(d) = v;
  dot(M) = 0 - q; // TODO: support unary -
  dot(v) = g - ISP * g * q / M;
}

subject_to {
  M_min < M; // TODO: add support for GT
  d <= d_final; // not crashed
  q <= max_q;
  0 <= q;
}

initially {
  d == 0;
  v == 0;
  M == 10000;
}

finally {
  d >= d_final - d_margin;
}


