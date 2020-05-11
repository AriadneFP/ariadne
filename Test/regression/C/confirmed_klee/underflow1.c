double foo(double x) {
  double array[4] = {0.001, 0.002, 0.003, 0.004};
  int i;
  double y = x;
  for (i=0; i <4; i++)
    y = y * y * array[i];
  return y;
}