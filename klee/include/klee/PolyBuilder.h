/*
 * PolyBuilder.h
 *
 *  Created on: Mar 23, 2011
 *      Author: thanh
 */

#ifndef POLYBUILDER_H_
#define POLYBUILDER_H_

#include "klee/Poly.h"
#include "klee/Expr.h"
#include "klee/Constraints.h"

namespace klee {
	class PolyBuilder {
	private:
		bool isBoundary( ref<Expr> exp );
		bool findFirstVar(
				const ConstraintManager constraints,ref<Expr>& result
		);
		bool findFirstVar(const ref<Expr> e,ref<Expr>& result);

	public:
		PolyBuilder(){}
		~PolyBuilder();
		//Converting to Polynomial Fraction form
		void toFraction (
			ref<Expr> exp, Poly &p, Poly &q, Poly::CompareType& type
		);

		bool linearizePoly(
			Poly p, Poly::CompareType type, ref<Expr> &var, ref<Expr>& oExpr
		);

		bool linearizePoly(
			const ref<Expr>iExpr, ref<Expr>& var, ref<Expr>& oExpr
		);

		bool linearize(const ref<Expr>iExpr, ref<Expr>& var, ref<Expr>& oExpr);

		bool linearize(
			const ConstraintManager iConstraints,
			ref<Expr>& var, ConstraintManager& oConstraints
		);

	};
}

#endif /* POLYBUILDER_H_ */
