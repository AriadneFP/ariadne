#include <gsl/gsl_sf_mathieu.h>
#include <gmp.h>
mpq_t deleteme;

/*struct result_struct{
  double x;
  double y;
};

typedef struct result_struct result;

struct genStruct_struct {
  result* res;
};

typedef struct genStruct_struct genStruct;*/

//double foo_global = 1;

//double foo_array[3] = { 1.0, 2.0, 3.0};

//static foo blah;

/*int somefunction(){
  double x = 1.0;
  foo dump;
  x = blah.x;
  foo_function(dump);
  return 0;
}*/

/*struct {int n; double f; long i; } fact_table[1] = {
    { 0,  1.0,     1L     },    
};*/
//double x = 1,0;

/*struct {int n; double f; } fact_table[2] = {
    { 0,  1.0},   
    { 0,  2.0}   
};*/

int foo () {
  gsl_matrix* matrix = gsl_matrix_alloc(1, 1);  
  return 0;
}

/*int main(void) {  
  //double data[3] = {1.0, 2.0, 3.0}; 
  foo data = {1, 1.0};
  //printf("x=%f\n",data.x);
  printf("Value: %d %f\n",data.a,data.x);
  return 0;
}*/