/*
 * Z3Builder.h
 *
 *  Created on: Mar 23, 2011
 *      Author: thanh
 */

#ifndef Z3BUILDER_H_
#define Z3BUILDER_H_

#include "klee/Constraints.h"
#include "z3.h"
#include "klee/SMTBuilder.h"

namespace klee {
  
  

  class Z3Builder : public SMTBuilder {
	private:
		Z3_config config;

	public:
		Z3Builder();
		~Z3Builder();

		void * getConfig() {
		  return (void *)config;
		}
		
		/// setTimeout - Set constraint solver timeout delay to the given value;
		/// 0 is off.
		void setTimeout(unsigned int timeout);

		//Obtain values for free variables
		//Updates as. ctx and m act as constants.
		void evalModel(void * ctx, void * m, AriadneSolver * as);
		
		//call to actual solver
		virtual SMT_result solver_check( void * ctx );
		virtual SMT_result solver_check_and_get_model( void * ctx, void ** model );
		
		virtual void * mk_context(void * config);
		virtual void del_context(void * context);
		virtual void del_model(void * context, void * model);
		virtual char * context_to_string(void * ctx);
		virtual void assert_constraint(void * ctx, void * ast);
		virtual void * mk_var( void * context, const char * name, void * sort_type );
		virtual void * mk_numeral(void * ctx, const char * value, void * real_sort);
		virtual void * mk_real_sort(void * ctx);
		
		//inequality operations
		virtual void * mk_eq(void * ctx, void * ast1, void * ast2);
		virtual void * mk_ne(void * ctx, void * ast1, void * ast2);
		virtual void * mk_lt(void * ctx, void * ast1, void * ast2);
		virtual void * mk_le(void * ctx, void * ast1, void * ast2);
		virtual void * mk_gt(void * ctx, void * ast1, void * ast2);
		virtual void * mk_ge(void * ctx, void * ast1, void * ast2);
		
		//bitwise operations
		virtual void * mk_not(void * ctx, void * ast1);
		virtual void * mk_and(void * ctx, void * ast1, void * ast2);
		virtual void * mk_or( void * ctx, void * ast1, void * ast2);
		virtual void * mk_xor(void * ctx, void * ast1, void * ast2);
		
		//arithmetic operations
		virtual void * mk_add( void * ctx, void * ast1, void * ast2);
		virtual void * mk_sub( void * ctx, void * ast1, void * ast2);
		virtual void * mk_mul( void * ctx, void * ast1, void * ast2);
		virtual void * mk_div(void * ctx, void * ast1, void * ast2);
		virtual void * mk_true(void * ctx);
		virtual void * mk_false( void * ctx);
		
  };
}


#endif /* Z3BUILDER_H_ */
