//===-- Poly.h --------------------------------------------------*- C++ -*-===//
//
//                     The ARIADNE Floating point exceptions detection
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef KLEE_POLY_H
#define KLEE_POLY_H
#include <iostream>
#include <fstream>

namespace klee{

class Poly{

private:
	double* coefs;
	int degree;

public:
	//Covert all types of expressions into the rational of polynomials
	//There are 4 possible formats
	//P(x)/Q(x) > 0
	//P(x)/Q(x) = 0
	//P(x)/Q(x) != 0
	//or P(x)/Q(x)
	//static std::ofstream logFile;
	enum CompareType{
		None = -1,
		Eq = 0,
		Gt = 1,
		Lt = 2,
		Ne = 3,
		Ge = 4,
		Le = 5
	};

	static CompareType reverseSign(CompareType type){
		switch(type){
		case Poly::Eq:
			return Poly::Eq;
		case Poly::Ne:
			return Poly::Ne;
		case Poly::Gt:
			return Poly::Lt;
		case Poly::Ge:
			return Poly::Le;
		case Poly::Lt:
			return Poly::Gt;
		case Poly::Le:
			return Poly::Ge;
		case Poly::None:
			return Poly::None;

		}
		return Poly::None;
	}

	Poly(const Poly& p){
		degree = p.getDegree();
		coefs = new double[degree+1];
		for(int i=0; i <= degree; i++)
			coefs[i] = p.getCoef(i);
	}

	static CompareType negateType(CompareType type){
		switch (type){
		case Poly::Eq:
			return Poly::Ne;
		case Poly::Ne:
			return Poly::Eq;
		case Poly::Gt:
			return Poly::Le;
		case Poly::Ge:
			return Poly::Lt;
		case Poly::Le:
			return Poly::Gt;
		case Poly::Lt:
			return Poly::Ge;
		case Poly::None:
			return Poly::None;
		}
		return Poly::None;
	}

	Poly(double* c, int n): coefs(c), degree(n) {}

	int getDegree() const {
		return degree;
	}

	void setDegree(int n){
		degree = n;
	}

	void setCoef(int i, double x){
		coefs[i] = x;
	}

	double getCoef(int i) const{
		if (i>= 0 && i <= degree)
			return coefs[i];
		else
			return 0;
	}

	Poly(const int n){
		degree = n;
		if (degree >= 0) coefs = new double[degree+1];
		for (int i = 0; i< n+1; i++)
			coefs[i] = 0;
	}

	double* getCoefs() const{
		return coefs;
	}

	Poly(){
		coefs = NULL;
		degree = -1;
	}

	~Poly(){

		if (coefs)
			delete [] coefs;
		coefs = NULL;
		degree = -1;
	}

	//inline Poly operator=(const Poly& rhs){
	Poly operator=(const Poly& rhs){
		if (this != &rhs) {
			this->degree = rhs.getDegree();
			this->coefs = new double[degree+1];
			for(int i=0; i <= degree; i++)
				this->coefs[i] = rhs.getCoef(i);
		}
		return *this;
	}

	Poly add(const Poly p);
	Poly subtract(const Poly p);
	Poly multiply(const Poly p);
	Poly div(const double denom);

	static Poly mk_Const(const double c){
		Poly *p = new Poly(0);
		p->coefs[0] = c;
		return *p;
	}

	static Poly mk_Var(){
		Poly *p = new Poly(1);
		p->coefs[0] = 0.0;
		p->coefs[1] = 1.0;
		return *p;
	}

	void print(std::ostream& os);
	void print(std::ostream &os, CompareType type);
	//Check if there is any coefficient is Nan
	bool isNan();
	//Simplify the polynomials when the coefficient of the degree expression is 0
	void simplify();
	void normalize(CompareType& type);
	bool findRoots (double* &root,int &num );
	static void simplify(Poly &p, Poly &q);

	/*
	 * Normalize the polynomial fraction such that the coefficients of both
	 * nom and denom polynomials is less than or equal to 1.0. Then, we do not
	 * get the Inf coefficients when comparing with DBL_MAX or DBL_MIN
	 */

	static void normalize(Poly &nom, Poly &denom);

};
}
#endif
