#include <stdio.h>
void setBound(){
  char* bound_env = getenv("__ARIADNE_BOUNDMAX");
  int value = 8;
  if (bound_env == NULL) 
    value = atoi(bound_env);
}