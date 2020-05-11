double ariadne_foo (double x, double y) {
  double z = sqrt(x*x + y *y);
  if ( z > 1.0)
    printf("Large!\n");
  else
    printf("Small!\n");
}