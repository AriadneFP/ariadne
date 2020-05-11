#include <stdio.h>
#include <math.h>
double add (double x,double y){
  double z = x + y;  
  return z;
}

double subtract (double x, double y){
  return (x - y);
}

double multiply (double x, double y){
  return ( x * y );
}

double divide (double x, double y){
  return ( x / y );
}

int main(void){
  double x = -1.0;
  double y = -1.0;
  double z = x + y;
  double u = x * y;
  double v = sqrt(u);
  //u = v + 1.0;
  /*if (u > 1.0)
    printf("Large!");*/
  return 0;  
}
