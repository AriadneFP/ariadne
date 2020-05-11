/*
 * PolyBuilder.cpp
 *
 *  Created on: Mar 23, 2011
 *      Author: thanh
 */

#include "klee/PolyBuilder.h"
#include "klee/Expr.h"
#include "klee/Solver.h"
#include "klee/AriadneSolver.h"
#include "klee/Constraints.h"
#include "klee/Poly.h"
#include "float.h"

/// Ariadne
#include "klee/util/AriadneCommon.h"

using namespace klee;
using namespace llvm;

// Check if an expresion is DBL_MAX or -DBL_MAX
bool PolyBuilder::isBoundary( ref<Expr> exp ){
	if (ConstantExpr *C = dyn_cast<ConstantExpr> (exp)){
		double value = C->getAPValue().bitsToDouble();
		if (value >= DBL_MAX || value <= -DBL_MAX)
			return true;
	}
	return false;
}

//Convert to polynomial fraction form
void PolyBuilder::toFraction(
		ref<Expr> exp, Poly& nom, Poly& denom, Poly::CompareType& type
) {
	if (ConstantExpr *CE = dyn_cast<ConstantExpr>(exp)){
	  	denom = Poly::mk_Const(1.0);
		/*
	  	 * When the constant is inf, set it to DBL_MAX
	  	 * When the constant is -inf, set it to -DBL_MAX
	  	 */
	  	double value = CE->getAPValue().bitsToDouble();
	  	if (value > DBL_MAX)
	  		value = DBL_MAX;
	  	else if (value < -DBL_MAX)
	  		value = -DBL_MAX;
	  	nom = Poly::mk_Const(value);
		type = Poly::None;
		return;
	}
	if(exp->getKind()==Expr::Concat){
		denom = Poly::mk_Const(1.0);
		nom = Poly::mk_Var();
		type = Poly::None;
		return;
	}
	Poly nom1, nom2, denom1, denom2;
	Poly::CompareType t1,t2;
	switch (exp-> getKind()){

	//FIXME: Consider the cases comparing with DBL_MAX or -DBL_MAX
	//Comparison Expressions
	case Expr::Slt:
		toFraction(exp->getKid(0), nom1, denom1, t1);
		toFraction(exp->getKid(1), nom2,denom2,t2);
		if (isBoundary(exp->getKid(0)))
			Poly::normalize(nom2, denom2);
		if (isBoundary(exp->getKid(1)))
			Poly::normalize(nom1, denom1);
		nom = nom1.multiply(denom2).subtract(nom2.multiply(denom1));
	  	denom = denom1.multiply(denom2);
	  	type = Poly::Lt;
		break;
	case Expr::Ult:
		toFraction(exp->getKid(0), nom1, denom1, t1);
		toFraction(exp->getKid(1), nom2, denom2, t2);
		if (isBoundary(exp->getKid(0)))
			Poly::normalize(nom2, denom2);
		if (isBoundary(exp->getKid(1)))
			Poly::normalize(nom1, denom1);
		nom = nom1.multiply(denom2).subtract(nom2.multiply(denom1));
		denom = denom1.multiply(denom2);
		type = Poly::Lt;
	  	break;
	case Expr::Eq:
		if (exp->getKid(0)->isBool() || exp->getKid(1)->isBool()){

			if (ConstantExpr *CE = dyn_cast<ConstantExpr>(exp->getKid(0))){
				if (CE->isTrue()){
					toFraction(exp->getKid(1), nom, denom, type);
					return;
				} else {
					toFraction(exp->getKid(1), nom, denom, type);
					type = Poly::negateType(type);
					return;
				}
			}
			if(ConstantExpr *CE = dyn_cast<ConstantExpr>(exp->getKid(1))){
				if (CE->isTrue()){
					toFraction(exp->getKid(0), nom, denom, type);
					return;
				}else{
					toFraction(exp->getKid(0), nom,denom,type);
					type = Poly::negateType(type);
					return;
				}
			}

		}else{
			toFraction(exp->getKid(0), nom1, denom1, t1);
			toFraction(exp->getKid(1), nom2, denom2, t2);
			if (isBoundary(exp->getKid(0)))
				Poly::normalize(nom2, denom2);
			if (isBoundary(exp->getKid(1)))
				Poly::normalize(nom1, denom1);
			nom = nom1.multiply(denom2).subtract(nom2.multiply(denom1));
			denom = denom1.multiply(denom2);
			type = Poly::Eq;
		}
	  	break;
	case Expr::Ne:
	  	toFraction(exp->getKid(0), nom1, denom1, t1);
	  	toFraction(exp->getKid(1), nom2, denom2, t2);
	  	if (isBoundary(exp->getKid(0)))
	  		Poly::normalize(nom2, denom2);
	  	if (isBoundary(exp->getKid(1)))
	  		Poly::normalize(nom1, denom1);
	  	nom = nom1.multiply(denom2).subtract(nom2.multiply(denom1));
	  	denom = denom1.multiply(denom2);
	  	type = Poly::Ne;
	  	break;

	case Expr::Sgt:
	  	toFraction(exp->getKid(0), nom1, denom1, t1);
	  	toFraction(exp->getKid(1), nom2, denom2, t2);
	  	if (isBoundary(exp->getKid(0)))
	  		Poly::normalize(nom2, denom2);
	  	if (isBoundary(exp->getKid(1)))
	  		Poly::normalize(nom1, denom1);
	  	nom = nom1.multiply(denom2).subtract(nom2.multiply(denom1));
	  	denom = denom1.multiply(denom2);
	  	type = Poly::Gt;
	  	break;
	case Expr::Ugt:
	  	toFraction(exp->getKid(0),nom1,denom1,t1);
	  	toFraction(exp->getKid(1),nom2,denom2,t2);
	  	if (isBoundary(exp->getKid(0)))
	  		Poly::normalize(nom2, denom2);
	  	if (isBoundary(exp->getKid(1)))
	  		Poly::normalize(nom1, denom1);
	  	nom = nom1.multiply(denom2).subtract(nom2.multiply(denom1));
	  	denom = denom1.multiply(denom2);
	  	type = Poly::Gt;
	  	break;

  	//Arithmetic
	case Expr::Add:
		toFraction(exp->getKid(0), nom1,denom1,t1);
		toFraction(exp->getKid(1), nom2,denom2,t2);
	  	nom = nom1.multiply(denom2).add(nom2.multiply(denom1));
	  	denom = denom1.multiply(denom2);
	  	type = Poly::None;
		break;
	case Expr::Sub:
	  	toFraction(exp->getKid(0), nom1, denom1, t1);
	  	toFraction(exp->getKid(1), nom2, denom2, t2);
	  	nom = (nom1.multiply(denom2)).subtract(nom2.multiply(denom1));
	  	denom = denom1.multiply(denom2);
	  	type = Poly::None;
	  	break;
	case Expr::Mul:
	  	toFraction(exp->getKid(0), nom1, denom1, t1);
		toFraction(exp->getKid(1), nom2, denom2, t2);
	  	nom = nom1.multiply(nom2);
	  	denom = denom1.multiply(denom2);
	  	type = Poly::None;
	  	break;
	case Expr::UDiv:
	  	toFraction(exp->getKid(0), nom1, denom1, t1);
	  	toFraction(exp->getKid(1), nom2, denom2, t2);
	  	nom = nom1.multiply(denom2);
	  	denom = denom1.multiply(nom2);
	  	type = Poly::None;
	  	break;
	case Expr::SDiv:
	  	toFraction(exp->getKid(0), nom1, denom1, t1);
	  	toFraction(exp->getKid(1), nom2, denom2,t2);
	  	nom = nom1.multiply(denom2);
	  	denom = denom1.multiply(nom2);
	  	type = Poly::None;
	  	break;
	case Expr::Div:
	  	toFraction(exp->getKid(0), nom1,denom1,t1);
	  	toFraction(exp->getKid(1), nom2,denom2,t2);
	  	nom = nom1.multiply(denom2);
	  	denom = denom1.multiply(nom2);
	  	type = Poly::None;
	  	break;

	default:
  		nom = Poly::mk_Const(0.0);
  		denom = Poly::mk_Const(0.0);
  		type = Poly::None;
  	  	break;
	}
	nom.simplify();
	denom.simplify();
	Poly::simplify(nom,denom);
}


bool PolyBuilder::linearizePoly(
	Poly p, Poly::CompareType compare, ref<Expr> &var, ref<Expr>& oExpr
) {

	ref<Expr> LHS, RHS, constant1, constant2, exp, next, qexp;
	p.normalize(compare);
	double* roots;
	int count, num;
	bool success = true;

	// Case: the polynomial has degree = 0
	if (p.getDegree() == 0)
		num = 0;
	// Case: the polynomial has degree = 1
	else if (p.getDegree() == 1){
		num = 1;
		roots = new double[1];
		roots[0] = - p.getCoef(1) / p.getCoef(0);
	} else
		success = p.findRoots(roots, num);
	if (!success){
		return false;
	}
	count = num - 1;
	Poly::CompareType cmp;
	if (p.getCoef(p.getDegree())< 0)
		cmp = Poly::reverseSign(compare);
	else
		cmp = compare;

	/*
	 * In case we can not find roots,
	 * thus the polynomials always is greater or less than zero
	 */
	if (num == 0){
		switch(cmp){
		case Poly::Eq:
			oExpr = ConstantExpr::alloc(0,Expr::Bool);
			return true;
		case Poly::Ne:
			oExpr = ConstantExpr::alloc(1,Expr::Bool);
			return true;
		case Poly::Ge:
			oExpr = ConstantExpr::alloc(1,Expr::Bool);
			return true;
		case Poly::Gt:
			oExpr = ConstantExpr::alloc(1,Expr::Bool);
			return true;
		case Poly::Lt:
			oExpr = ConstantExpr::alloc(0,Expr::Bool);
			return true;
		case Poly::Le:
			oExpr = ConstantExpr::alloc(0,Expr::Bool);
			return true;
		default:
			oExpr = ConstantExpr::alloc(1,Expr::Bool);
			return true;
		}
	}
	//In case there is only one root
	if (num == 1){
		constant1 = cast<Expr>(ConstantExpr::alloc(llvm::APFloat(roots[0])));
		switch (cmp){
			case Poly::Eq:
				oExpr = cast<Expr>(EqExpr::create(var,constant1));
				return true;
			case Poly::Ne:
				oExpr = cast<Expr>(NeExpr::create(var,constant1));
				return true;
			case Poly::Lt:
				oExpr = cast<Expr>(SltExpr::create(var,constant1));
				return true;
			case Poly::Le:
				oExpr = cast<Expr>(SleExpr::create(var,constant1));
				return true;
			case Poly::Gt:
				oExpr = cast<Expr>(SgtExpr::create(var,constant1));
				return true;
			case Poly::Ge:
				oExpr = cast<Expr>(SgeExpr::create(var,constant1));
				return true;
			default:
				oExpr = ConstantExpr::alloc(1,Expr::Bool);
				return true;
		}
	}
	switch (cmp){
		case Poly::Eq:
			constant1 = cast<Expr>(ConstantExpr::alloc(
					llvm::APFloat(roots[0]))
			);
			exp = cast<Expr>(EqExpr::create(var,constant1));
			for (int i= 1; i < num; i++){
				constant1 = cast<Expr>(ConstantExpr::alloc(
						llvm::APFloat(roots[i]))
				);
				next = cast<Expr>(EqExpr::create(var,constant1));
				exp = cast<Expr>(OrExpr::create(exp,next));
			}
			oExpr = exp;
			break;

		case Poly::Ne:
			constant1 = cast<Expr>(ConstantExpr::alloc(
					llvm::APFloat(roots[0]))
			);
			exp = cast<Expr>(NeExpr::create(var,constant1));
			for (int i= 1; i < num; i++){
				constant1 = cast<Expr>(ConstantExpr::alloc(
						llvm::APFloat(roots[i]))
				);
				next = cast<Expr>(NeExpr::create(var,constant1));
				exp = cast<Expr>(AndExpr::create(exp,next));
			}
			oExpr = exp;
			break;

		case Poly::Lt:
			constant1 = cast<Expr>(ConstantExpr::alloc(
					llvm::APFloat(roots[count]))
			);
			constant2 = cast<Expr>(ConstantExpr::alloc(
					llvm::APFloat(roots[count-1]))
			);
			LHS = cast<Expr> (SltExpr::create(constant2,var));
			RHS = cast<Expr> (SltExpr::create(var,constant1));
			exp = cast<Expr> (AndExpr::create(LHS,RHS));
			count = count - 2;
			while (count >= 0){
				if (count == 0){
					constant1 = cast<Expr>(
							ConstantExpr::alloc(llvm::APFloat(roots[count]))
					);
					next = cast<Expr>(SltExpr::create(var,constant1));
					exp = cast<Expr>(OrExpr::create(exp,next));
					break;
				}else{
					constant1 = cast<Expr>(
							ConstantExpr::alloc(llvm::APFloat(roots[count]))
					);
					constant2 = cast<Expr>(
							ConstantExpr::alloc(llvm::APFloat(roots[count-1]))
					);
					LHS = cast<Expr>(SltExpr::create(constant2,var));
					RHS = cast<Expr>(SltExpr::create(var,constant1));
					next = cast<Expr> (AndExpr::alloc(LHS,RHS));
					exp = cast<Expr>(OrExpr::create(exp,next));
					count = count -2;
				}
			}
			oExpr = exp;
			break;

		case Poly::Le:
			constant1 = cast<Expr>(
					ConstantExpr::alloc(llvm::APFloat(roots[count]))
			);
			constant2 = cast<Expr>(
					ConstantExpr::alloc(llvm::APFloat(roots[count-1]))
			);
			LHS = cast<Expr>(SleExpr::create(constant2,var));
			RHS = cast<Expr>(SleExpr::create(var,constant1));
			exp = cast<Expr> (AndExpr::alloc(LHS,RHS));
			count = count - 2;
			while (count >= 0){
				if (count == 0){
					constant1 = cast<Expr>(
							ConstantExpr::alloc(llvm::APFloat(roots[count]))
					);
					next = cast<Expr>(SleExpr::create(var,constant1));
					exp = cast<Expr>(OrExpr::create(exp,next));
					break;
				}else {
					constant1 = cast<Expr>(
							ConstantExpr::alloc(llvm::APFloat(roots[count]))
					);
					constant2 = cast<Expr>(
							ConstantExpr::alloc(llvm::APFloat(roots[count-1]))
					);
					LHS = cast<Expr>(SleExpr::create(constant2,var));
					RHS = cast<Expr>(SleExpr::create(var,constant1));
					next = cast<Expr> (AndExpr::alloc(LHS,RHS));
					exp = cast<Expr>(OrExpr::create(exp,next));
					count = count -2;
				}
			}
			oExpr = exp;
			break;
		case Poly::Gt:
			constant1 = cast<Expr>(
					ConstantExpr::alloc(llvm::APFloat(roots[count]))
			);
			exp = cast<Expr> (SgtExpr::create(var,constant1));
			count = count - 1;
			while (count >= 0){
				if (count == 0){
					constant1 = cast<Expr>(
							ConstantExpr::alloc(llvm::APFloat(roots[count]))
					);
					next = cast<Expr>(SltExpr::create(var,constant1));
					exp = cast<Expr>(OrExpr::create(exp,next));
					break;
				}else{
					constant1 = cast<Expr>(
							ConstantExpr::alloc(llvm::APFloat(roots[count]))
					);
					constant2 = cast<Expr>(
							ConstantExpr::alloc(llvm::APFloat(roots[count-1]))
					);
					LHS = cast<Expr>(SltExpr::create(constant2,var));
					RHS = cast<Expr>(SltExpr::create(var,constant1));
					next = cast<Expr> (AndExpr::alloc(LHS,RHS));
					exp = cast<Expr>(OrExpr::create(exp,next));
					count = count -2;
				}
			}
			oExpr = exp;
			break;
		case Poly::Ge:
			constant1 = constant1 = cast<Expr>(
					ConstantExpr::alloc(llvm::APFloat(roots[count]))
			);
			exp = cast<Expr> (SgeExpr::create(var,constant1));
			count = count - 1;
			while (count >= 0){
				if (count == 0){
					constant1 = cast<Expr>(
							ConstantExpr::alloc(llvm::APFloat(roots[count]))
					);
					next = cast<Expr>(SleExpr::create(var,constant1));
					exp = cast<Expr>(OrExpr::create(exp,next));
					break;
				}else{
					constant1 = cast<Expr>(
							ConstantExpr::alloc(llvm::APFloat(roots[count]))
					);
					constant2 = cast<Expr>(
							ConstantExpr::alloc(llvm::APFloat(roots[count-1]))
					);
					LHS = cast<Expr>(SleExpr::create(constant2,var));
					RHS = cast<Expr>(SleExpr::create(var,constant1));
					next = cast<Expr> (AndExpr::alloc(LHS,RHS));
					exp = cast<Expr>(OrExpr::create(exp,next));
					count = count - 2;
				}
			}
			oExpr = exp;
			break;

		default:
			break;
		}
	return true;
}

bool PolyBuilder::findFirstVar(const ref<Expr> e,ref<Expr>& result){
	if (e->getKind() == Expr::Concat){
		result = e;
	    return true;
	}else {
		Expr *ep = e.get();
	    for (unsigned i=0; i < ep->getNumKids(); i++)
		   if(findFirstVar(ep->getKid(i),result))
			   return true;
	}
	return false;
}

bool PolyBuilder::findFirstVar(const ConstraintManager constraints,ref<Expr>& result){
	bool res = false;
	for (ConstraintManager::const_iterator it = constraints.begin(),
		    ie = constraints.end(); it != ie; ++it) {
			res = findFirstVar(*it,result);
			if (res)
				return res;
	}
	return res;
}


bool PolyBuilder::linearizePoly(
	const ref<Expr>iExpr, ref<Expr>& var, ref<Expr>& oExpr
) {

	/// If the expression is linear expression, just return the original one
	if (ariadneSolver->isLinear(iExpr)) {
		oExpr = iExpr;
		return true;
	}

	Poly nom,denom,p;
	Poly::CompareType type;
	bool res = findFirstVar(iExpr,var);

	/*
	 * Find roots of final polynomial and convert constraints
	 * into a set of linear constraints
	 */
	iExpr->toPolyFraction(nom, denom, type);
	/*if (nom.getDegree() + denom.getDegree() <=1){
		oExpr = iExpr;
		return true;
	}*/
	ref<Expr> LHS, RHS, exp1, exp2, exp3, exp4;
	res = linearizePoly(nom, type, var, exp1) && res;
	res = linearizePoly(denom, Poly::Gt, var, exp2) && res;
	res = linearizePoly(nom, Poly::reverseSign(type), var, exp3) && res;
	res = linearizePoly(denom, Poly::Lt, var, exp4) && res;
	if (!res)
		return false;
	LHS = cast<Expr>(AndExpr::create(exp1,exp2));
	RHS = cast<Expr>(AndExpr::create(exp3,exp4));
	oExpr = cast<Expr>(OrExpr::create(LHS,RHS));
	return res;
}

bool PolyBuilder::linearize(
	const ref<Expr>iExpr, ref<Expr>& var, ref<Expr>& oExpr
) {
	ref<Expr> LHS,RHS,exp;
	bool success = true;
	switch(iExpr->getKind()){
	case Expr::Eq:
		if (iExpr->getKid(0)->isBool()){
			success = linearize(iExpr->getKid(1),var,RHS);
			if (!success)
				return false;
			oExpr = EqExpr::create(iExpr->getKid(0),RHS);
		} else if (iExpr->getKid(1)->isBool()){
			success = linearize(iExpr->getKid(0),var,LHS);
			if (!success)
				return false;
			oExpr = EqExpr::create(LHS,iExpr->getKid(1));
		}else {
			success = linearizePoly(iExpr,var,oExpr);
			if (!success)
				return false;
		}
		break;
	case Expr::Ne:
		if (iExpr->getKid(0)->isBool() || iExpr->getKid(1)->isBool()){
			success = linearize(iExpr->getKid(0),var,LHS);
			if (!success)
				return false;
			success = linearize(iExpr->getKid(1),var,RHS);
			if(!success)
				return false;
			oExpr = NeExpr::create(LHS,RHS);
		}else {
			success = linearizePoly(iExpr,var,oExpr);
			if(!success)
				return false;
		}
		break;
	case Expr::And:
		success = linearizePoly(iExpr->getKid(0),var,LHS);
		if(!success)
			return false;
		success = linearizePoly(iExpr->getKid(1),var,RHS);
		if(!success)
			return false;
		oExpr = AndExpr::create(LHS,RHS);
		break;
	case Expr::Or:
		success = linearizePoly(iExpr->getKid(0),var,LHS);
		if (!success)
			return false;
		success = linearizePoly(iExpr->getKid(1),var,RHS);
		if (!success)
			return false;
		oExpr = OrExpr::create(LHS,RHS);
		break;
	case Expr::Xor:
		success = linearizePoly(iExpr->getKid(0),var,LHS);
		if (!success)
			return false;
		success = linearizePoly(iExpr->getKid(1),var,RHS);
		if (!success)
			return false;
		oExpr = XorExpr::create(LHS,RHS);
		break;
	case Expr::Not:
		linearize(iExpr->getKid(0),var,exp);
		oExpr = NotExpr::create(exp);
		break;
	default:
		return linearizePoly(iExpr,var,oExpr);
		//break;
	}
	return true;
}

bool PolyBuilder::linearize(
	const ConstraintManager iConstraints,
	ref<Expr>& var, ConstraintManager& oConstraints
) {
	bool success;
	for (ConstraintManager::const_iterator it = iConstraints.begin(),
			ie = iConstraints.end(); it != ie; it++) {
		ref<Expr> expr = *it;
		ref<Expr> exp;
		success = linearize(expr, var, exp);
		if (!success)
			return false;
		oConstraints.addConstraint(exp);
	}
	return true;
}
