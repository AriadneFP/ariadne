#include "z3.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

int main( void ) {
	Z3_config config = Z3_mk_config();
	Z3_set_param_value ( config, "SOFT_TIMEOUT", "10" );
	Z3_context ctx = Z3_mk_context( config );

	Z3_parse_smtlib_file( ctx, "context.smt",
			0, 0, 0,
			0, 0, 0);
	unsigned i, num_formulas;
	num_formulas = Z3_get_smtlib_num_formulas(ctx);

	for (i = 0; i < num_formulas; i++) {
		Z3_ast f = Z3_get_smtlib_formula(ctx, i);
		printf("formula %d: %s\n", i, Z3_ast_to_string(ctx, f));
		Z3_assert_cnstr(ctx, f);
	}

	Z3_lbool res = Z3_check(ctx);
	if (res == Z3_L_TRUE)
		printf("True\n");
	else if (res == Z3_L_FALSE)
		printf("False\n");
	else
		printf("Unknown\n");
}
