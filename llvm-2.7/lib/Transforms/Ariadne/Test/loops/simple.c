#include <math.h>
int sum(){
    int i,sum;
    double x,y,z;
    x = 1.0;
    y = 0.0;
    z = log(x);
    sum = 0;
    for (i = 0; i < 10; i++){
      sum += i;
    }
    return sum;
}