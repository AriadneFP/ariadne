#include <stdio.h>
#include <float.h>
double av1(double x, double y){
	double z;
	z = (x+y)/2.0;
	return z;
}
double av2(double x, double y){
	double z;	
	z = x/2.0 + y/2.0;
	return z;
}
double av3(double x,double y){
	double z;
	z = x + (y-x)/2.0;
	return z;
}
double av4(double x, double y){
	double z;
	z = y + (x-y)/2.0;
	return z;
}
double average(double x, double y){
	int samesign;
	double avg;
	if ( x >= 0 ){
		if (y >=0)
			samesign = 1;
		else
			samesign = 0;	
	}else{
		if (y >= 0)
			samesign = 0;
		else
			samesign = 1;	
	}
	if (samesign){
		if ( y >= x)
			avg = av3(x,y);
		else
			avg = av4(x,y);	
	}else
		avg = av1(x,y);
	return avg;	
}