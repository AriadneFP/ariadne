#include <fenv.h>
int main(void) {
  feenableexcept (
	FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW
  );
  double x = 0; 
  double y = 0;
  printf("%f", x/y);
  //double z = x/y;
  return 0;
}