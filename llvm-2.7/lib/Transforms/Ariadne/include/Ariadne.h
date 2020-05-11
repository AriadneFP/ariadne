#include <stdio.h>
#ifndef ARIADNE	
#define ARIADNE
double fabs (double x){
  if (x >= 0)
    return x;
  else
    return -x;
}
/*void setBound(){
  char* bound_env = getenv("__ARIADNE_BOUNDMAX");
  int value = 8;
  if (bound_env == NULL) 
    value = atoi(bound_env);
}*/
#endif
