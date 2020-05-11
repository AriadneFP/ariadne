int foo_call(int *i ) {
  int j = *i + 1;
  printf ("%d\n", j);  
  return j;
}

double db_foo(double *x) {
  printf( "%f\n", *x);
  return *x;
}

int main(void) {
  //int i = 1;
  //int i;
  //klee_make_symbolic(&i, sizeof(i),"i");
  //foo_call(i);
  //double x = 1.0;
  //db_foo(x);
  int i = 1;
  foo_call(&i);
  return 1;
}