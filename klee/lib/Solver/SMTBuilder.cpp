/* 
 * SMTBuilder.cpp
 *
 *  Created on: August 26, 2011
 *  Author: Jonathan Hollenbeck
 */

#include "klee/SMTBuilder.h"

#include <float.h>

using namespace klee;
using namespace llvm;
using namespace std;

SMTBuilder::SMTBuilder(){
}

//Extract ReadExpr from a Concat expression
const ReadExpr* SMTBuilder::extractReadExpr(ref<Expr> e) {
    assert(e->getKind() == Expr::Concat);

    const ReadExpr *base = dyn_cast<ReadExpr>(e->getKid(0));

    // right now, all Reads are byte reads but some
    // transformations might change this
    if (!base)
      return false;
    return base;
}

bool SMTBuilder::isbool(const ref<Expr> exp){
	if (exp.isNull())
		return true;
	bool result = false;
	switch (exp->getKind()){

		/// Comparison Operations
	    case Expr::Slt:
			result = true;
			break;
		case Expr::Ult:
			result = true;
			break;
		case Expr::Ule:
			result = true;
			break;
		case Expr::Sle:
			result = true;
			break;

		case Expr::Sge:
			result = true;
			break;
		case Expr::Uge:
			result = true;
			break;
		case Expr::Sgt:
			result = true;
			break;
		case Expr::Ugt:
			result = true;
			break;

		case Expr::Eq:
			result = true;
			break;
		case Expr::Ne:
			result = true;
			break;

		/// Bitwise arithmetic operations
		case Expr::Not:
			result = true;
			break;
		case Expr::And:
			result = true;
			break;
		case Expr::Or:
			result = true;
			break;
		case Expr::Xor:
			result = true;
			break;
		default:
			break;

	}
	return result;
}


void SMTBuilder::collectSelectExprs(
	ref<Expr> e, std::set<SelectExpr> &selectExprList,
	std::set<int> &selectIndexList
) {
	if (SelectExpr *selectExpr = dyn_cast< SelectExpr >(e)) {
		if (selectIndexList.find(selectExpr->getSymbolicIndex()) == selectIndexList.end()) {
			selectIndexList.insert(selectExpr->getSymbolicIndex());
			selectExprList.insert(*selectExpr);
		}
		return;
	} else {
		Expr *ep = e.get();
		for (unsigned i = 0; i < ep->getNumKids(); i++) {
			collectSelectExprs((ep->getKid(i)), selectExprList, selectIndexList);
		}
	}
}

void SMTBuilder::conditionConstraints(ConstraintManager &constraints, AriadneSolver * as) {
  
  //Preprocess: Adding preconditions for the constraints
  for (int num = 1; num <= as->getNumVar(); num++) {
		std::ostringstream s;
		s << "arr" << num;
		Array* array = new Array(s.str(), Expr::Int64);
		ref<Expr> var = Expr::createTempRead(array, Expr::Int64);
		ref<Expr> constant1 = cast<Expr> (ConstantExpr::alloc(llvm::APFloat(
				DBL_MAX)));
		ref<Expr> constant2 = cast<Expr> (ConstantExpr::alloc(llvm::APFloat(
				-DBL_MAX)));
		ref<Expr> LHS = cast<Expr> (SleExpr::create(var, constant1));
		ref<Expr> RHS = cast<Expr> (SleExpr::create(constant2, var));
		ref<Expr> max = cast<Expr> (AndExpr::create(LHS, RHS));
		constant1 = cast<Expr> (ConstantExpr::alloc(llvm::APFloat(DBL_MIN)));
		constant2 = cast<Expr> (ConstantExpr::alloc(llvm::APFloat(-DBL_MIN)));
		LHS = cast<Expr> (SgeExpr::create(var, constant1));
		RHS = cast<Expr> (SgeExpr::create(constant2, var));
		ref<Expr> min = cast<Expr> (OrExpr::create(LHS, RHS));
		ref<Expr> zero = cast<Expr> (ConstantExpr::alloc(llvm::APFloat(0.0)));
		ref<Expr> isZero = cast<Expr> (EqExpr::create(var, zero));
		ref<Expr> expr = cast<Expr> (OrExpr::create(min, isZero));
		expr = cast<Expr> (AndExpr::create(max, expr));
		constraints.addConstraint(expr);
	}
	/// Generate current path constraints to check Z3 hang issue
	/*ofstream constraintsFile("currentConstraints.smt");
	ExprPPrinter::printSMT_LIB(constraintsFile, constraints);*/
}


void * SMTBuilder::getAst(
	void * context, const ref<Expr> exp, SMT_sort_kind skind
) {
	if (exp.isNull())
		return NULL;
	void * ast; void * ast1 = NULL; void * ast2 = NULL;

	void * real_sort = mk_real_sort(context);
	if (ConstantExpr *CE = dyn_cast<ConstantExpr>(exp)){
		std::string value;
		/*
		 * When the constant is inf, set it to DBL_MAX
		 * When the constant is -inf, set it to -DBL_MAX
		 */
		CE->toString(value);
		/// Inf case
		if (value.find("+Inf") != string::npos) {
			CE = dyn_cast<ConstantExpr> (
					ConstantExpr::alloc(APFloat(DBL_MAX))
			);
			CE->toString(value);
		}
		/// -Inf case
		else if (value.find("-Inf") != string::npos) {
			CE = dyn_cast<ConstantExpr> (
					ConstantExpr::alloc(APFloat(DBL_MAX))
			);
			CE->toString(value);
		}
		/// NaN case
		else if (value.find("NaN") != string::npos) {
			return NULL;
		}

		if (skind == SMT_REAL_SORT){
			int num = value.length();
			std::string denom;
			std::string nom;
			bool firstNum = false;
			for (int i = 0; i < num ; i++ ) {
				// Ignore . in the expression
				if (value.at(i) == '.')
					continue;
				else if (value.at(i)=='0' && firstNum == false)
					continue;
				else {
					nom.push_back(value.at(i));
					if (firstNum == false && value.at(i) != '-')
						firstNum = true;
				}
			}
			int count = 0;
			bool isInt = true;
			for (int i=0; i < num; i++){
				if (value.at(i) == '.'){
					isInt = false;
					count = num - i;
					break;
				}
			}
			if (isInt){
			  ast = mk_numeral(context, value.c_str(), real_sort);
			}else{
				denom.push_back('1');
				for (int i = 1; i < count; i++)
					denom.push_back('0');
				ast1 = mk_numeral(context, nom.c_str(), real_sort);
				ast2 = mk_numeral(context, denom.c_str(), real_sort);
				ast = mk_div(context, ast1, ast2);
			}
		} else {
			if(value.compare("0") == 0)
			  ast = mk_false(context);
			else
			  ast = mk_true(context);
		}
		return ast;
	} else {
	     if ( exp->getKind() == Expr::Concat) {
	    	const ReadExpr *base = extractReadExpr(exp);
	    	ast = mk_var(context, base->updates.root->name.c_str(), real_sort);
	    	return ast;
	     }
	}
	SMT_sort_kind type;
	type = SMT_REAL_SORT;
	switch( exp->getKind()){

	default:
		ExprPPrinter::printSingleExpr(std::cout, exp); // debugging
		ast = getAst(context, exp->getKid(0), type);
		break;
	case Expr::Eq:
		if( isbool(exp->getKid(0)) || isbool(exp->getKid(1)))
			type = SMT_BOOL_SORT;
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_eq(context, ast1, ast2);
		break;
	case Expr::Ne:
		if( isbool(exp->getKid(0)) || isbool(exp->getKid(1)))
			type = SMT_BOOL_SORT;
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_ne(context, ast1, ast2);
		break;
	case Expr::Slt:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_lt(context, ast1, ast2);
		break;
	case Expr::Ult:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_lt(context, ast1, ast2);
		break;
	case Expr::Sle:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_le(context, ast1, ast2);
		break;
	case Expr::Ule:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_le(context, ast1, ast2);
		break;
	case Expr::Sgt:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_gt(context, ast1, ast2);
		break;
	case Expr::Ugt:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_gt(context, ast1, ast2);
		break;
	case Expr::Uge:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_ge(context, ast1, ast2);
		break;
	case Expr::Sge:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_ge(context, ast1, ast2);
		break;

	/// Bitwise Operations
	case Expr::Not:
		type = SMT_BOOL_SORT;
		ast1 = getAst(context, exp->getKid(0), type);
		if (!ast1)
			return NULL;
		ast = mk_not(context, ast1);
		break;
	case Expr::And:
		type = SMT_BOOL_SORT;
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_and(context, ast1, ast2);
		break;
	case Expr::Or:
		type = SMT_BOOL_SORT;
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_or(context, ast1, ast2);
		break;
	case Expr::Xor:
		type = SMT_BOOL_SORT;
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_xor(context, ast1, ast2);
		break;

	//Arithmetic operations
	case Expr::Add:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_add(context, ast1, ast2);
		break;
	case Expr::Sub:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_sub(context, ast1, ast2);
		break;
	case Expr::Mul:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_mul(context, ast1, ast2);
		break;
	case Expr::UDiv:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_div(context, ast1, ast2);
		break;
	case Expr::SDiv:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_div(context, ast1, ast2);
		break;
	case Expr::Div:
		ast1 = getAst(context, exp->getKid(0), type);
		ast2 = getAst(context, exp->getKid(1), type);
		if (!ast1 || !ast2)
			return NULL;
		ast = mk_div(context, ast1, ast2);
		break;
	case Expr::Select:
		int index = 0;
		if (SelectExpr *selectExpr = dyn_cast< SelectExpr >(exp))
			index = selectExpr->getSymbolicIndex();
		std::stringstream ss;
		ss << "select" << index;
		ast = mk_var(context, ss.str().c_str(), real_sort);
		break;

	}
	return ast;
}

SMT_result SMTBuilder::constructSMT (
				   ConstraintManager constraints, void * context
				   ,  AriadneSolver * as) {
	std::set<SelectExpr> selectExprList;
	std::set<int> selectIndexList;

	/*
	 * With constraints containing sqrt variables, do not add preconditions. The
	 * preconditions make the constraints more complex and the solver can not
	 * give us the solutions.
	 */
	if (!constraints.containSqrtVars())
		conditionConstraints(constraints, as);

	/// Collect and print the list of all select instructions
	for (ConstraintManager::const_iterator it = constraints.begin(),
	     ie = constraints.end(); it != ie; ++it)
		collectSelectExprs(*it, selectExprList, selectIndexList);

	/// Assert the conditions for select expressions
	for (
		std::set<SelectExpr>::iterator it = selectExprList.begin();
		it != selectExprList.end(); it++
	) {
		SelectExpr selectExpr = *it;
		int index = selectExpr.getSymbolicIndex();
		std::stringstream ss;
		ss << "select" << index;

		void * eq1 = NULL; void * eq2 = NULL;
		void * arg1 = NULL; void * arg2 = NULL;

		void * ast = mk_var(context, ss.str().c_str(), mk_real_sort(context));
		void * cond = getAst(context, selectExpr.getKid(0), SMT_BOOL_SORT);
		void * trueAst = getAst(context, selectExpr.getKid(1), SMT_REAL_SORT);
		void * falseAst = getAst(context, selectExpr.getKid(2), SMT_REAL_SORT);
		
		if (!cond || !trueAst || !falseAst)
			continue;

		eq1 = cond; eq2 = mk_true(context);
		arg1 = mk_eq(context, eq1, eq2);
		eq1 = ast; eq2 = trueAst;
		arg2 = mk_eq(context, eq1, eq2);
		void * fact1 = mk_and(context, arg1, arg2);
		eq1 = cond, eq2 = mk_false(context);
		arg1 = mk_eq(context, eq1, eq2);
		eq1 = ast, eq2 = falseAst;
		arg2 = mk_eq(context, eq1, eq2);
		void * fact2 = mk_and(context, arg1, arg2);
		void * fact = mk_or(context, fact1, fact2);
		assert_constraint(context, fact);
	}

	for (ConstraintManager::const_iterator it = constraints.begin(),
	         ie = constraints.end(); it != ie; ++it) {
		ref<Expr> exp = *it;
		if (ConstantExpr *C = dyn_cast<ConstantExpr> (exp)){
			if (C->isFalse()) {
				return ST_UNDEF;
			} else if (C->isTrue()) {
				continue;
			}
		}
		else {
		  void *formula = getAst(context, exp, SMT_BOOL_SORT);
		  /// Ignore illegal AST
		  if (formula)
			  assert_constraint(context, formula);
		}
	}
	return ST_TRUE;
}

SMT_result SMTBuilder::evaluateSMT( const ConstraintManager constraints, AriadneSolver * as) {

  void * context = mk_context( getConfig() );
  SMT_result status = constructSMT( constraints, context, as);

  ofstream contextFile("context.z3");
  char * contextStr = context_to_string(context);
  contextFile << contextStr << endl;
  contextFile.close();
  return status;
}

SMT_result SMTBuilder::SMT_check( ConstraintManager constraints, AriadneSolver * as) {
        // Pre-processing
        void * context = mk_context( getConfig() );
	SMT_result status;
	status = constructSMT( constraints, context, as );
	if (status == ST_UNDEF) {
	        del_context( context );
		return status;
	}
	else if (status == ST_FALSE) {
		del_context( context );
		return status;
	}
	
	status = solver_check( context );
	del_context( context );
	return status;
}

SMT_result SMTBuilder::SMT_check_and_get_model( ConstraintManager constraints, AriadneSolver * as) {
	// Pre-processing
    void * context = mk_context( getConfig() );
	SMT_result status;
	status = constructSMT( constraints, context, as );
	if (status == ST_UNDEF) {
		del_context( context );
		return status;
	}
	else if (status == ST_FALSE) {
		del_context( context );
		return status;
	}
	
	void * model;
	status = solver_check_and_get_model( context, &model );

	if (status == SMT_L_TRUE) {
	  evalModel( context, model, as );
	}
	
	del_model( context, model );
	del_context( context );
	return status;
}
