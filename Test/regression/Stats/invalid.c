#include<stdio.h>

double divide(double x, double y) {
  return (x/y);
}

double div1(double x, double y) {
  double z = x /(y + 1.0) + 1.0;
  return z;
}

double div2(double x, double y) {
  double z = ( y*x / (x*x*x - 1) );
  return z;
}