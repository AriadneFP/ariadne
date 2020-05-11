/*int main(int argc, char *argv[]){
  double *value;  
  if (argc <= 1) {
    printf("No arguments for the program!\n");    
    return 0;
  }
  value = (double*) malloc(argc-1);  
  int i;
    
  for (i = 1; i < argc; i++) {
    value[i-1] = 0.0;    
  }  
  return 0;
}*/

void foo () {
  /*const int num =10;
  double *value;
  value = (double *) malloc(num);*/
  double value[10];
  value[0] = 1.0;
}

int main(void) {
  foo();
  return 0;
}