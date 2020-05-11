float foo1(float x) {
  return x;
}

float foo2(float x) {
  return foo1(x);
}

int foo (double x, double *y, double *t) {
    double k = *t;
    double z = *y + 1.0;
    return 1;
}