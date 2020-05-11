double ariadne_sqrt(double x){
  if ( x < 0 ){
    printf ("Invalid!\n");
    exit(0);
  }
  double y;
  klee_make_symbolic(&y,sizeof y,"y");
  if (x == 0 && y == 0)
    return y;
  if (x == 1 && y == 1)
    return y;
  int check;
  check = (0 < x) && ( x < 1) && ( 0 < y ) && ( y < 1 ) && ( y*y == x);
  if (check)
    return y;
  check = (x > 1) && (y > 1) && ( y*y == x);
  if (check)
    return y;
  else{
    printf("Infeasible path!\n");
    exit(0);
  }
  return y;       
}