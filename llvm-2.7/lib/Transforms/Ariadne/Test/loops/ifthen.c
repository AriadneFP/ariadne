#include <math.h>

int main() {
	int i,j, sum;
	double x,y,z;
	x = 1.0;
	y = 0.0;
	z = log(x);
	sum = 0;
	for (i = 0; i < 10; i++) {
		if (z < 0)
			x = 2.0;
		else
			x = 3.0;
	}
	return sum;
}
