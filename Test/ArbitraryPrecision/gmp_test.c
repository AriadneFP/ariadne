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
  value = (mpq_t*) malloc(argc-1);  
  int i;
    
  for (i = 1; i < argc; i++) {
    mpq_init(value[i-1]);
    mpq_set_str(value[i-1], argv[i], 10);    
  }  
  return 0;
}