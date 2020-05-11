/* 
 * SMTBuilder.h
 *
 *  Created on: August 26, 2011
 *  Author: Jonathan Hollenbeck
 */

#ifndef SMTBUILDER_H_
#define SMTBUILDER_H_

#include "klee/AriadneSolver.h"

namespace klee { 

  //replaces Z3_sort_kind. used to represent types
  enum SMT_sort_kind {
    SMT_REAL_SORT = 0,
    SMT_BOOL_SORT
  };

  class SMTBuilder {
  public:
    SMTBuilder();
    ~SMTBuilder();

    /// setTimeout - Set constraint solver timeout delay to the given value;
    /// 0 is off.
    virtual void setTimeout(unsigned int timeout) = 0;
    //Updates as. context and model act as constants.
    virtual void evalModel(void * context, void * model, AriadneSolver * as) = 0;
    virtual void * getConfig() = 0;

    const ReadExpr* extractReadExpr( ref<Expr> e );
    bool isbool( const ref<Expr> exp );

    /// Collect select expressions
    void collectSelectExprs(
			    ref<Expr> e, std::set<SelectExpr> &selectExprList,
			    std::set<int> &selectIndexList
			    );

    void conditionConstraints(ConstraintManager &constraints, AriadneSolver * as);

    // construct constraints with an SMT solver
    SMT_result constructSMT( ConstraintManager constraints, void * ctx , AriadneSolver * as);
    void * getAst( void * context, const ref<Expr> exp,SMT_sort_kind skind );
    SMT_result evaluateSMT(const ConstraintManager constraints, AriadneSolver * as);
		
    SMT_result SMT_check(ConstraintManager constraints, AriadneSolver * as);
    SMT_result SMT_check_and_get_model( ConstraintManager constraints, AriadneSolver * as);

    //call to actual solver
    virtual SMT_result solver_check( void * ctx ) = 0;
    virtual SMT_result solver_check_and_get_model( void * ctx, void ** model) = 0;

    virtual void * mk_context(void * config) = 0;
    virtual void del_context(void * context) = 0;
    virtual void del_model(void * context, void * model) = 0;
    virtual char * context_to_string(void * ctx) = 0;
    virtual void assert_constraint(void * ctx, void * ast) = 0;
    virtual void * mk_var( void * context, const char * name, void * sort_type ) = 0;
    virtual void * mk_numeral(void * ctx, const char * value, void * real_sort) = 0;
    virtual void * mk_real_sort(void * ctx) = 0;
    
    //inequality operations
    virtual void * mk_eq(void * ctx, void * ast1, void * ast2) = 0;
    virtual void * mk_ne(void * ctx, void * ast1, void * ast2) = 0;
    virtual void * mk_lt(void * ctx, void * ast1, void * ast2) = 0;
    virtual void * mk_le(void * ctx, void * ast1, void * ast2) = 0;
    virtual void * mk_gt(void * ctx, void * ast1, void * ast2) = 0;
    virtual void * mk_ge(void * ctx, void * ast1, void * ast2) = 0;
    
    //bitwise operations
    virtual void * mk_not(void * ctx, void * ast1) = 0;
    virtual void * mk_and(void * ctx, void * ast1, void * ast2) = 0;
    virtual void * mk_or( void * ctx, void * ast1, void * ast2 ) = 0;
    virtual void * mk_xor(void * ctx, void * ast1, void * ast2) = 0;
    
    //arithmetic operations
    virtual void * mk_add( void * ctx, void * ast1, void * ast2 )= 0;
    virtual void * mk_sub( void * ctx, void * ast1, void * ast2 ) = 0;
    virtual void * mk_mul( void * ctx, void * ast1, void * ast2 ) = 0;
    virtual void * mk_div(void * ctx, void * ast1, void * ast2) = 0;
    virtual void * mk_true(void * ctx) = 0;
    virtual void * mk_false( void * ctx) = 0;
    
  };
}

#endif /* SMTBUILDER_H_ */
