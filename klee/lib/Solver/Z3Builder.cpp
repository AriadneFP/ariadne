/*
 * Z3Builder.cpp
 *
 *  Created on: Mar 23, 2011
 *      Author: thanh
 */

#include "klee/Z3Builder.h"

using namespace klee;
using namespace llvm;
using namespace std;

Z3Builder::Z3Builder() {

	/// Z3 configuration for Boogie
	config = Z3_mk_config();
	Z3_set_param_value(config, "CASE_SPLIT", "3");
	Z3_set_param_value(config, "DELAY_UNITS", "true");
	Z3_set_param_value(config, "QI_EAGER_THRESHOLD", "100");
	Z3_set_param_value(config, "RESTART_STRATEGY", "0");
	Z3_set_param_value(config, "RESTART_FACTOR", "1.5");
	Z3_set_param_value(config, "AUTO_CONFIG", "false");
}

void Z3Builder::setTimeout(unsigned int timeout) {

	stringstream out;
	out << timeout;
	string timeoutStr = out.str();
	
	Z3_set_param_value(config, "MODEL", "true");
	Z3_set_param_value(config, "MODEL_ON_TIMEOUT", "true");
	Z3_set_param_value(config, "SOFT_TIMEOUT", timeoutStr.c_str());

}

void * Z3Builder::mk_var(void * context, const char * name, void * sort_type)
{
  Z3_symbol s = Z3_mk_string_symbol( (Z3_context)context, name);
  return (void *)Z3_mk_const((Z3_context)context, s, (Z3_sort)sort_type);
}

//Updates as. context and model act as constants.
void Z3Builder::evalModel(void * context, void * model, AriadneSolver * as) {

  //cast variables appropriately
  Z3_context ctx = (Z3_context)context;
  Z3_model m = (Z3_model)model;

	unsigned num_constants = Z3_get_model_num_constants(ctx, m);

	/// This is from Z3_API examples
	for (unsigned i = 0; i < num_constants; i++) {
	    Z3_symbol name;
	    Z3_func_decl cnst = Z3_get_model_constant(ctx, m, i);
	    Z3_ast a, v;
	    Z3_bool ok;
	    name = Z3_get_decl_name(ctx, cnst);
	    std::string var;
	    switch(Z3_get_symbol_kind(ctx, name)){
	    case Z3_STRING_SYMBOL:
	        var = std::string( Z3_get_symbol_string(ctx, name) );
	        break;
		default:
			break;
	    }
	    a = Z3_mk_app(ctx, cnst, 0, 0);
	    v = a;
	    ok = Z3_eval(ctx, m, a, &v);

		std::set<std::string>::iterator it;

		switch ( Z3_get_ast_kind(ctx, v) ){
	    case Z3_NUMERAL_AST:
			it = as->namesList.find(var);
			if (it != as->namesList.end()) {
				if (!as->concretizing[var])
					as->assignment[var] = std::string(
						Z3_get_numeral_string(ctx, v)
					);
			} else{
				if(v) {
					as->namesList.insert(var);
					//Set value as string
					if (!as->concretizing[var])
						as->assignment.insert(
							std::pair<std::string, std::string>(
								var, std::string(Z3_get_numeral_string(ctx, v))
							)
						);

				}
			}
	    	break;
	    default:
	    	std::cerr << "do not handle this type of AST" << std::endl;
	    	break;
	    }
	}
	
	context = (void *)ctx;
	model = (void *)m;
}

//call to actual solver
SMT_result Z3Builder::solver_check( void * ctx ) {
  return (SMT_result)Z3_check( (Z3_context)ctx );
}

SMT_result Z3Builder::solver_check_and_get_model( void * ctx, void ** model ) {
  return (SMT_result)Z3_check_and_get_model( (Z3_context)ctx, (Z3_model *)model ) ; 
}
  
void * Z3Builder::mk_context(void * config) {
  return (void *)Z3_mk_context( (Z3_config)config);
}

void Z3Builder::del_context(void * context) {
  Z3_del_context( (Z3_context)context );
}

void Z3Builder::del_model(void * context, void * model) {
  Z3_del_model( (Z3_context)context, (Z3_model)model );
}

char * Z3Builder::context_to_string(void * ctx) {
  return (char *)Z3_context_to_string( (Z3_context)ctx ); 
}
		
void Z3Builder::assert_constraint(void * ctx, void * ast) {
  Z3_assert_cnstr( (Z3_context)ctx, (Z3_ast)ast );
}
		
void * Z3Builder::mk_var( void * context, const char * name, void * sort_type );

void * Z3Builder::mk_numeral(void * ctx, const char * value, void * real_sort) {
  return (void *)Z3_mk_numeral( (Z3_context)ctx, (Z3_string)value, (Z3_sort)real_sort );
}

void * Z3Builder::mk_real_sort(void * ctx) {
  return (void *)Z3_mk_real_sort( (Z3_context)ctx );
}

//inequality operations
void * Z3Builder::mk_eq(void * ctx, void * ast1, void * ast2) {
  return (void *)Z3_mk_eq( (Z3_context)ctx, (Z3_ast)ast1, (Z3_ast)ast2 );
}

void * Z3Builder::mk_ne(void * ctx, void * ast1, void * ast2) {
  Z3_ast args[2] = { (Z3_ast)ast1, (Z3_ast)ast2 };
  return (void *)Z3_mk_distinct( (Z3_context)ctx, 2, args );
}

void * Z3Builder::mk_lt(void * ctx, void * ast1, void * ast2) {
  return (void *)Z3_mk_lt( (Z3_context)ctx, (Z3_ast)ast1, (Z3_ast)ast2 );
}

void * Z3Builder::mk_le(void * ctx, void * ast1, void * ast2) {
  return (void *)Z3_mk_le( (Z3_context)ctx, (Z3_ast)ast1, (Z3_ast)ast2 );
}

void * Z3Builder::mk_gt(void * ctx, void * ast1, void * ast2) {
  return (void *)Z3_mk_gt( (Z3_context)ctx, (Z3_ast)ast1, (Z3_ast)ast2 );
}

void * Z3Builder::mk_ge(void * ctx, void * ast1, void * ast2) {
  return (void *)Z3_mk_ge( (Z3_context)ctx, (Z3_ast)ast1, (Z3_ast)ast2 );
}
		
//bitwise operations
void * Z3Builder::mk_not(void * ctx, void * ast1)
{
  return (void *)Z3_mk_not( (Z3_context)ctx, (Z3_ast)ast1 );
}

void * Z3Builder::mk_and(void * ctx, void * ast1, void * ast2) {
  Z3_ast args[2] = { (Z3_ast)ast1, (Z3_ast)ast2 };
  return (void *)Z3_mk_and( (Z3_context)ctx, 2, args );
}

void * Z3Builder::mk_or( void * ctx, void * ast1, void * ast2 ) {
  Z3_ast args[2] = { (Z3_ast)ast1, (Z3_ast)ast2 };
  return (void *)Z3_mk_or( (Z3_context)ctx, 2, args );
}

void * Z3Builder::mk_xor(void * ctx, void * ast1, void * ast2) {
  return (void *)Z3_mk_xor( (Z3_context)ctx, (Z3_ast)ast1, (Z3_ast)ast2 );
}
		
//arithmetic operations
void * Z3Builder::mk_add( void * ctx, void * ast1, void * ast2 ) {
  Z3_ast args[2] = { (Z3_ast)ast1, (Z3_ast)ast2 };
  return (void *)Z3_mk_add( (Z3_context)ctx, 2, args );
}

void * Z3Builder::mk_sub( void * ctx, void * ast1, void * ast2 ) {
  Z3_ast args[2] = { (Z3_ast)ast1, (Z3_ast)ast2 };
  return (void *)Z3_mk_sub( (Z3_context)ctx, 2, args );
}

void * Z3Builder::mk_mul( void * ctx, void * ast1, void * ast2 ) {
  Z3_ast args[2] = { (Z3_ast)ast1, (Z3_ast)ast2 };
  return (void *)Z3_mk_mul( (Z3_context)ctx, 2, args );
}

void * Z3Builder::mk_div(void * ctx, void * ast1, void * ast2) {
  return (void *)Z3_mk_div( (Z3_context)ctx, (Z3_ast)ast1, (Z3_ast)ast2 );
}

void * Z3Builder::mk_true(void * ctx) {
  return (void *)Z3_mk_true( (Z3_context)ctx );
}
		
void * Z3Builder::mk_false( void * ctx) {
  return (void *)Z3_mk_false( (Z3_context)ctx );
}
