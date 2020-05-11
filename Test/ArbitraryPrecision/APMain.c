/* This program will take a function name and return the mpq_t value 
 * stored in the file
 */
 
#include <gmp.h>
#include <stdio.h>
#include <stdlib.h>
int main(int argc, char *argv[]){
  mpq_t *value;  
  if (argc <= 1) {
    printf("No arguments for the program!\n");    
    return 0;
  }
  printf("%d\n",argc-1);
  value = (mpq_t*) malloc(argc-1);  
  int i;  
  for (i = 1; i < argc; i++) {
    mpq_t tmp;
    mpq_init(tmp);
    mpq_set_str(tmp, argv[i], 10);
    mpq_init(value[i-1]);
    mpq_set(value[i-1],tmp);
    printf("%s\n",argv[i]);    
    double x = mpq_get_d(value[i-1]);
    printf("%f\n",x);
  }
  
  /*for (i =1; i < argc; i++) {
    mpq_clear(value[i-1]);
  }*/
  return 0;
}