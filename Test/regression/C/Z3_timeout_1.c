#include "z3.h"

int main( void ) {
  Z3_config config = Z3_mk_config( );
  Z3_set_param_value ( config, "SOFT_TIMEOUT", "10" );
  Z3_context ctx = Z3_mk_context( config );
  Z3_parse_smtlib_file( ctx, "context.smt", 0, 0, 0, 0, 0, 0);
  unsigned i;
  for (i = 0; i < Z3_get_smtlib_num_formulas( ctx ); i++) {
        Z3_ast f = Z3_get_smtlib_formula( ctx, i );
        Z3_assert_cnstr( ctx, f );
  }
  Z3_lbool res =  Z3_check( ctx );
  switch(res) {
      case Z3_L_TRUE:
	printf("SAT\n");
	break;
      case Z3_L_FALSE:
	printf("UNSAT\n");
	break;
      default:
	printf("UNKNOWN\n");
	break;
  }
}
