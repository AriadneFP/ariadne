#include <math.h>

int main() {
	int i,j, sum;
	double x,y,z;
	x = 1.0;
	y = 0.0;
	z = log(x);
	sum = 0;
	for (i = 0; i < 10; i++) {
		for (j = 0; j < 15; ++j) {
			sum += i;
		}
	}
	return sum;
}
