static double dsqrarg;
#define DSQR(a) ((dsqrarg=(a)) == 0.0 ? 0.0 : dsqrarg*dsqrarg)
double pythag(double a, double b){
  a=fabs(a);	
  b=fabs(b);
  if( a>b )
    return a*sqrt(1.0+DSQR(b/a));
  if ( b==0.0 )
    return 0.0;
  return b*sqrt(1.0+DSQR(a/b));
}
