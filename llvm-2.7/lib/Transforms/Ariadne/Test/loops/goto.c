#include <math.h>

int main() {
		int i,sum;
		double x,y,z;
		x = 1.0;
		if (x > 0) goto LOOP;
		y = 0.0;
		z = log(x);
		sum = 0;
LOOP:	for (i = 0; i < 10; i++){
			sum += i;
		}
		if (z < 1) goto LOOP;
		return sum;
}
