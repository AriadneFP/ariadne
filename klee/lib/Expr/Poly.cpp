//===-- Poly.cpp ----------------------------------------------------------===//
//
//                     The ARIADNE Floating Point Exception Detection
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include <config.h>
#include <stdlib.h>
#include "klee/Poly.h"
#include <gsl/gsl_poly.h>
#include <gsl/gsl_errno.h>
#include <float.h>
#include <math.h>

using namespace klee;


Poly Poly::add(const Poly p){
    int n;
    if (this->getDegree() > p.getDegree())
    	n = this->getDegree();
    else
    	n = p.getDegree();
    Poly q = Poly(n);
    for (int i =0 ; i <= n; i++){
    	q.setCoef(i,this->getCoef(i) + p.getCoef(i));
    }
    q.simplify();
	return q;
}
Poly Poly::subtract(const Poly p){
	int n;
	if (this->getDegree()> p.getDegree())
	   	n = this->getDegree();
	else
	  	n = p.getDegree();
	Poly q = Poly(n);
	for (int i = 0 ; i <= n; i++){
	  	double tmp = this->getCoef(i) - p.getCoef(i);
		q.setCoef(i,tmp);
	}
	q.simplify();
	return q;
}
Poly Poly::multiply(const Poly p){
	int n = this->getDegree() + p.getDegree();
	Poly q = Poly(n);
	for(int i = 0; i <= n; i++){
		double tmp = 0;
		for (int j = 0; j <= i; j++){
			tmp += this->getCoef(j) * p.getCoef(i-j);
		}
		q.setCoef(i,tmp);
	}
	q.simplify();
	return q;
}

Poly Poly::div(const double denom){
	int n = this->getDegree();
	Poly q = Poly(n);
	for (int i = 0; i <= n; i++){
		double tmp = 0;
		tmp = this->getCoef(i)/denom;
		q.setCoef(i,tmp);
	}
	q.simplify();
	return q;
}

void Poly::print(std::ostream &os){
	if (degree ==0){
			os << getCoef(degree);
			os << std::endl;
			return;
	}
	os << getCoef(degree) << "x^" << degree;

	for(int i = degree-1; i >= 1 ; i-- ){
		if (getCoef(i)> 0){
			os << "+" << getCoef(i) << "x^" << i;
		}else if (getCoef(i)< 0 ){
			os << getCoef(i) << "x^" << i;
		}
	}
	if (getCoef(0) > 0)
		os << "+" << getCoef(0);
	if (getCoef(0) < 0)
		os << getCoef(0);
	//os << std::endl;
}

void Poly::print(std::ostream &os, CompareType type){
	print(os);
	switch (type){
	case Poly::Eq:
		os << " = 0";
		break;
	case Poly::Ne:
		os << " != 0";
		break;
	case Poly::Gt:
		os << " > 0";
		break;
	case Poly::Ge:
		os << " >= 0";
		break;
	case Poly::Le:
		os << " <= 0";
		break;
	case Poly::Lt:
		os << " < 0";
		break;
	default:
		break;
	}
	os << std::endl;
	return;
}

void Poly::simplify(){
	if (degree < 0) return;

	double *tmp = new double[degree +1];
	for (int i=0; i<=degree; i++)
		tmp[i] = coefs[i];
	for (int i = degree; i>=0; i--){
		if (getCoef(i)!=0){
			degree = i;
			break;
		}
		if (i == 0)
			degree = 0;
	}
	if(coefs != NULL)
		delete [] coefs;
	coefs = new double[degree+1];
	for (int i = 0; i<=degree; i++){
		if (tmp[i] > DBL_MAX)
			coefs[i] = DBL_MAX;
		else if (tmp[i]< -DBL_MAX)
			coefs[i] = -DBL_MAX;
		else
			coefs[i] = tmp[i];
	}
	delete [] tmp;

}
int minnonzero(Poly& p){
	int min = 0;
	while(min <= p.getDegree()){
		if (p.getCoef(min) != 0)
			break;
		else
			min++;
	}
	return min;
}
void reduce(Poly p,int const m){
	int nDegree = p.getDegree() - m;
	if (nDegree <= 0)
		return;
	Poly *q = new Poly(nDegree);
	for (int i = 0; i <= nDegree; i++){
		q->setCoef(i,p.getCoef(i+m));
	}
	p = *q;
}
void Poly::simplify(Poly& p, Poly& q){
	int minp = minnonzero(p);
	int minq = minnonzero(q);
	int min;
	if (minp > minq)
		min = minq;
	else
		min = minp;
	if (min > 0){
		reduce(p,min);
		reduce(q,min);
	}
}

void Poly::normalize(Poly &nom, Poly &denom){
	double max = 0.0;
	for (int i = 0; i <= nom.getDegree(); i++){
		if (max < fabs(nom.getCoef(i)))
				max = fabs(nom.getCoef(i));
	}
	for (int i = 0; i <= denom.getDegree(); i++){
		if (max < fabs(denom.getCoef(i)))
				max = fabs(denom.getCoef(i));
	}

	for (int i = 0; i <= nom.getDegree(); i++){
		nom.coefs[i] /= max;
	}

	for (int i = 0; i <= denom.getDegree(); i++){
		denom.coefs[i] /= max;
	}

}

void Poly::normalize(Poly::CompareType& type){
	double denom = coefs[degree];
	if (denom > 0){
		for (int i = 0; i <= degree; i++){
			coefs[i] = coefs[i]/denom;
		}
	}
	if (denom < 0){
		type = Poly::reverseSign(type);
		for (int i = 0; i <= degree; i++){
			coefs[i] = coefs[i]/denom;
		}
	}
	this->simplify();
}

bool Poly::isNan(){
	for (int i = 0; i < degree; i++){
		if (isnan(getCoef(i)))
				return true;
	}
	return false;
}


bool Poly::findRoots(double* &roots, int &num ){
	//In case the poly is invalid
	if (isNan())
		return false;
	gsl_poly_complex_workspace *w = gsl_poly_complex_workspace_alloc (degree +1);
	double *z = new double[2*degree];
	double *tmpRoots = new double[degree];
	int status;
	//The degree of the polynomial is 2
	if (degree == 2){
		num = gsl_poly_solve_quadratic(coefs[2],coefs[1],coefs[0],
											&tmpRoots[0],&tmpRoots[1]);
		if (num == 1){
			tmpRoots[1] = tmpRoots[0];
			num = 2;
		}
	}
	//Cubic polynomial
	if (degree == 3){
		num = gsl_poly_solve_cubic(coefs[2],coefs[1],coefs[0],
										&tmpRoots[0],&tmpRoots[1],&tmpRoots[2]);

	}
	//The degree of polynomial is larger than 3
	else{
		status = gsl_poly_complex_solve(coefs,(size_t)(degree+1),w,z);
		if (status == GSL_EFAILED)
			return false;
		gsl_poly_complex_workspace_free(w);
		num = 0;
		for (int i = 0; i < degree; i++ ){
			if (z[2*i+1] == 0 ){
				tmpRoots[num] = z[2*i];
				num++;
			}
		}
	}

	//FIXME: Need to process the roots here !!
	//Omit all even degree in root factors.
	for (int i = 0; i < num; i++){
		for (int j = i; j < num; j++){
			if ( tmpRoots[i] > tmpRoots[j]){
				double tmp = tmpRoots[i];
				tmpRoots[i] = tmpRoots[j];
				tmpRoots[j] = tmp;
			}

		}
	}
	roots = new double[num];
	for (int i =0; i < num; i++){
		roots[i] = tmpRoots[i];
	}
	delete [] tmpRoots;
	return true;
}

