struct foo {
  double x;
}
int main (void ) {
  double x = 1.0;
  double *y = &x;
  printf("%f\n",*y);
}