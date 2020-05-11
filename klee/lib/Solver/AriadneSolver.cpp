/*
 * AriadneSolver.cpp
 *
 *  Created on: Apr 5, 2011
 *      Author: thanh
 *
 *  This solver will use PolyBuider and Z3 Builder to solve the constraints.
 */

#include "../lib/Core/Common.h"

#include "SolverStats.h"

#include "klee/TimerStatIncrementer.h"
#include "klee/util/Assignment.h"
#include "klee/util/ExprUtil.h"
#include "klee/Internal/Support/Timer.h"
#include "klee/Internal/System/Time.h"
#include "llvm/System/Process.h"
#include "klee/Statistics.h"
#include "../lib/Core/CoreStats.h"

#include <cassert>
#include <cstdio>
#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <map>
#include <vector>

#include <errno.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <ctime>
#include <cstdlib>

#include <float.h>
#include "gmp.h"
#include "z3.h"
#include "klee/util/AriadneUtils.h"
#include "klee/util/AriadneCommon.h"
#include "klee/util/ExprUtil.h"
#include "klee/AriadneSolver.h"
#include "../fparser/fparser.hh"
#include "../fparser/fpconfig.hh"

#include "klee/AriadneSolver.h"
#include "klee/SMTBuilder.h"
#include "klee/Z3Builder.h"

using namespace klee;
using namespace llvm;
using namespace std;

int AriadneSolver::getRandInt() {
	return rand();
}

double AriadneSolver::getRandomValue() {
	long int M = 10000;
	double x,r;
	r = ( (double)getRandInt() / ((double)(RAND_MAX) + (double)(1))) - 1.0/2.0;
	x = (r*M);
	return x;
}

ConstantExpr AriadneSolver::getRandomConstExpr(){
	double x = getRandomValue();
	return *ConstantExpr::alloc(llvm::APFloat(x));
}

void AriadneSolver::setNumVar(int num){
    varNum = num;
    for(int i = 1; i <= varNum; i++) {
    	std::ostringstream var;
    	var << "arr" << i;
    	namesList.insert(var.str());
    	// Set default values
    	assignment.insert(
    		std::pair<std::string,std::string>(
    				var.str(),std::string("0")
    		)
    	);
    	// As default the variable is not set for a concrete value
    	concretizing.insert(
    		std::pair<std::string,bool>(var.str(), false)
    	);
   	}
}

bool AriadneSolver::find(ref<Expr> e,const std::string name){
	if (!isa<ConstantExpr>(e)) {
		if (e->getKind() == Expr::Concat){
			const ReadExpr *re = klee::extractReadExpr(e);
			if (re->updates.root->name.compare(name)==0){
				return true;
			}
		} else {
			Expr *ep = e.get();
			for (unsigned i=0; i < ep->getNumKids(); i++){
				bool res = find((ep->getKid(i)),name);
				if (res)
					return true;
			}
		}
	}
	return false;
}

bool AriadneSolver::find(
	ConstraintManager& constraints, const std::string name
) {
	for (ConstraintManager::const_iterator it = constraints.begin(),
			    ie = constraints.end(); it != ie; ++it) {
		if (find(*it,name))
			return true;
	}
	return false;
}

bool AriadneSolver::hasDependentVars(ConstraintManager &constraints) {
	for (int i = inputNum + 1; i <= varNum; i++ ) {
		std::string varName;
		std::stringstream varNameStream;
		varNameStream << "arr";
		varNameStream << i;
		varName = varNameStream.str();
		if (find(constraints, varName))
			return true;
	}
	return false;
}

int AriadneSolver::getFirstVarIndex(ConstraintManager constraints) {
	for (int i = 1; i<=varNum; i++ ) {
		std::string varName;
		std::stringstream varNameStream;
		varNameStream << "arr";
		varNameStream << i;
		varName = varNameStream.str();
		if (find(constraints,varName))
			return i;
	}
	return 1;
}

int AriadneSolver::getFirstVarIndex(ref<Expr> expr) {
	for (int i = 1; i<=varNum; i++ ) {
		std::string varName;
		std::stringstream varNameStream;
		varNameStream << "arr";
		varNameStream << i;
		varName = varNameStream.str();
		if (find(expr, varName))
			return i;
	}
	return 1;
}

bool AriadneSolver::isbool(const ref<Expr> exp){
	if (exp.isNull())
		return true;
	bool result = false;
	switch (exp->getKind()){
		//comparison arithmetic
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

		//bit arithmetic
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

bool AriadneSolverImpl::linearize(
	const ref<Expr>iExpr, ref<Expr>& var, ref<Expr>& oExpr
) {
	return polyBuilder->linearize(iExpr, var, oExpr);
}

bool AriadneSolverImpl::linearizeSingleVarExpr(
	const ref<Expr>iExpr, ref<Expr>& oExpr
) {
	int index = solver->getFirstVarIndex(iExpr);
	std::stringstream varNameStream;
	varNameStream << "arr";
	varNameStream << index;
	std::string varName = varNameStream.str();
	Array* array = new Array( varName.c_str(), Expr::Int64 );
	ref<Expr> var = Expr::createTempRead( array, Expr::Int64 );
	return polyBuilder->linearize(iExpr, var, oExpr);
}

bool AriadneSolver::linearize(
	const ConstraintManager iConstraints,
	ref<Expr>& var, ConstraintManager& oConstraints
) {
	return static_cast<AriadneSolverImpl*>(impl)->linearize(
		iConstraints, var, oConstraints
	);
}

bool AriadneSolver::linearize(
	const ref<Expr>iExpr, ref<Expr>& var, ref<Expr>& oExpr
) {
	return static_cast<AriadneSolverImpl*>(impl)->linearize(
			iExpr, var, oExpr
	);
}

bool AriadneSolverImpl::linearizeSingleVarConstraints(
	const ConstraintManager iConstraints, ConstraintManager& oConstraints
) {
	int index = solver->getFirstVarIndex(iConstraints);
	std::stringstream varNameStream;
	varNameStream << "arr";
	varNameStream << index;
	std::string varName = varNameStream.str();
	Array* array = new Array( varName.c_str(), Expr::Int64 );
	ref<Expr> var = Expr::createTempRead( array, Expr::Int64 );
	return polyBuilder->linearize ( iConstraints, var, oConstraints );
}

bool AriadneSolverImpl::linearize(
	const ConstraintManager iConstraints,
	ref<Expr>& var, ConstraintManager& oConstraints
) {
	return polyBuilder->linearize(iConstraints, var, oConstraints);
}

//Construct variables declaration
void AriadneSolver::declare(ref<Expr>e){
	if (!isa<ConstantExpr>(e)) {
	   Expr *ep = e.get();
	   for (unsigned i=0; i < ep->getNumKids(); i++)
	          declare(ep->getKid(i));
	   if (const ReadExpr *re = dyn_cast<ReadExpr>(e)) {
			std::set<std::string>::iterator it = namesList.find(
				re->updates.root->name
			);
	        if (it ==  namesList.end()) {
	        	namesList.insert(re->updates.root->name);
	        	usedExprs.insert(re);
	        }
	   }
	}
}

void AriadneSolver::evalModel(Z3_context ctx, Z3_model m) {

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

	    switch ( Z3_get_ast_kind(ctx, v) ) {

		case Z3_NUMERAL_AST:
			it = namesList.find(var);
			if (it != namesList.end()) {
				if (!concretizing[var])
					assignment[var] = std::string(
						Z3_get_numeral_string(ctx, v)
					);
			} else{
				if(v) {
					namesList.insert(var);
					//Set value as string
					if (!concretizing[var])
						assignment.insert(
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
}

/***/
AriadneSolver::AriadneSolver()
  : Solver(new AriadneSolverImpl(this))
{
	select = Exhaustive;
	varNum = 0;
	inputNum = 0;
	numOfIntervals = 10;
	checkTestCase = true; // As default, check test case
	print = false; // As default, we do not print out the input test cases
	z3_queries = 0;
	z3_sat_queries = 0;
	z3_unsat_queries = 0;
	poly_multiple_sat_queries = 0;
	poly_multiple_unsat_queries = 0;
	poly_single_sat_queries = 0;
	poly_single_unsat_queries = 0;
	poly_unsat_queries = 0;
	poly_sat_queries = 0;
	poly_queries = 0;
	maxConcretizations = 10;
	successfulConcretization = 0;
	failedConcretization = 0;
	queryNum = 0;
	exception = NotException;
	location = -1;
	satPaths = 0;
	unsatPaths = 0;
	unknownPaths = 0;
	useBoost = false;

	/// Z3 configuration for Boogie
	config = Z3_mk_config();
	Z3_set_param_value(config, "CASE_SPLIT", "3");
	Z3_set_param_value(config, "DELAY_UNITS", "true");
	Z3_set_param_value(config, "QI_EAGER_THRESHOLD", "100");
	Z3_set_param_value(config, "RESTART_STRATEGY", "0");
	Z3_set_param_value(config, "RESTART_FACTOR", "1.5");
	Z3_set_param_value(config, "AUTO_CONFIG", "false");
}

char* AriadneSolver::getConstraintLog(const Query &query) {
	return static_cast<AriadneSolverImpl*>(impl)->getConstraintLog(query);
}

// Get a random number in an interval
double AriadneSolver::getRandomValue(double start, double end) {
	assert(
		end >= start &&
		"In an interval, the starting point must less than or equal ending point"
	);
	double x,r;
	r = ( (double)getRandInt() / ((double)(RAND_MAX) + (double)(1)));
	x = start + r*(end -start);
	return x;
}

void AriadneSolver::setTimeout(unsigned int timeout) {
  timeOut = timeout;
  static_cast<AriadneSolverImpl*>(impl)->smtBuilder->setTimeout(timeout);
}

void AriadneSolver::printConstraints(
	ConstraintManager constraints, bool concretized, int success
) {
	std::string suffix;
	if (constraintType == AriadneSolver::ISAT)
		suffix = ".hys";
	else
		suffix = ".smt";
	/// Print out the path constraints collected when analyzing.
	std::string fileName;
	std::ofstream constraintFile;
	if (success > 0) {
		satPaths++;
		std::stringstream fileNameSS;
		fileNameSS << "SAT/PC" << satPaths << suffix;
		fileName = fileNameSS.str();
	} else if (success == 0) {
		/// After concretizing the constraints, we can not guarantee that
		/// the constraints are UNSAT
		if (concretized) {
			unknownPaths++;
			std::stringstream fileNameSS;
			fileNameSS << "UNKNOWN/PC" << unknownPaths << suffix;
			fileName = fileNameSS.str();
		} else {
			unsatPaths++;
			std::stringstream fileNameSS;
			fileNameSS << "UNSAT/PC" << unsatPaths << suffix;
			fileName = fileNameSS.str();
		}
	} else {
		unknownPaths++;
		std::stringstream fileNameSS;
		fileNameSS << "UNKNOWN/PC" << unknownPaths << suffix;
		fileName = fileNameSS.str();
	}
	constraintFile.open(fileName.c_str());
	if (constraintType == AriadneSolver::ISAT)
		ExprPPrinter::print_iSAT(constraintFile, constraints);
	else
		ExprPPrinter::printSMT_LIB(constraintFile, constraints);
	constraintFile.close();
}

// return 0 if the test cases fail to generate correct exception.
// return 1 if the test cases generates correct exception.
int AriadneSolver::checkTestCases (
	const std::vector<const Array*>	&objects,
	std::vector< std::vector<unsigned char> > &values
) {
	assert(!objects.empty()&& "The list of symbolic objects is empty!");

	std::string exceptionStr;

	switch (exceptionHandler->getException()) {
		case ExceptionHandler::Overflow:
			exceptionStr = "Potential Overflow";
			break;
		case ExceptionHandler::Underflow:
			exceptionStr = "Potential Underflow";
			break;
		case ExceptionHandler::Invalid:
			exceptionStr = "Potential Invalid";
			break;
		case ExceptionHandler::DivideByZero:
			exceptionStr = "Potential Division By Zero";
			break;
		default:
			return 1;
	}

	// Get the string inputs
	std::string inputs = "";
	for (
		std::vector<const Array*>::const_iterator
		it = objects.begin(), ie = objects.end(); it != ie; ++it
	) {
		const Array *array = *it;
		std::string prefix = "arr";
		if (!array->name.compare(0, prefix.size(), prefix))
			inputs += assignment[array->name] + " ";
	}

	exceptionStr += " ";
	std::ostringstream sStream;
	sStream << exceptionHandler->getLocation();
	exceptionStr += sStream.str();
	chomp(exceptionStr);
	/// Execute the inputs with concrete executable, and compare the output
	/// with the printed messages
	std::string command = "./test " + inputs;
	std::string output = exec(command.c_str());
	chomp(output);

	//reset the exception
	exceptionHandler->setException(ExceptionHandler::NotException);

	/// If the printed message is the same as the checking message, return 1
	/// mean that the exception is valid. Oherwise, the found exception is invalid
	if ( exceptionStr.compare(output.c_str()) == 0 )
		return 1;
	else
		return 0;
}

/// Always print inputs as rational formats
void AriadneSolver::printTestCases(
	const std::vector<const Array*>	&objects,
	std::vector< std::vector<unsigned char> > &values
) {
	for (
		std::vector<const Array*>::const_iterator
		it = objects.begin(), ie = objects.end(); it != ie; ++it
	) {
		const Array *array = *it;
		// Print value as rational string
		std::string valueStr = assignment[array->name];
		string prefix = "arr";
		if (print && !array->name.compare(0, prefix.size(), prefix)) {
			std::cout << array->name << " =" << std::endl;
			std::cout << valueStr << std::endl;
		}
		// Store input values in objects
		std::vector<unsigned char> data;
		data.reserve(array->size);
		mpq_t mpqValue;
		mpq_init( mpqValue );
		mpq_set_str( mpqValue, valueStr.c_str(), 10 );
		double dValue = mpq_get_d( mpqValue );
		unsigned char* val = reinterpret_cast<unsigned char*>(&dValue);
		for (unsigned offset = 0; offset < array->size; offset++) {
			data.push_back(val[offset]);
		}
		values.push_back(data);
	}
}

void AriadneSolver::concretize (
	ref<Expr> iExpr, int index, double value, ref<Expr>& oExpr
) {
	if (iExpr.isNull()) {
		oExpr = NULL;
		return;
	}
	if (iExpr->getKind() == Expr::Constant) {
		oExpr = iExpr;
		return;
	} else {
	    if ( iExpr->getKind() == Expr::Concat) {
	   	  const ReadExpr *base = klee::extractReadExpr(iExpr);
		  std::string name = base->updates.root->name;
		  std::ostringstream s;
		  s << "arr" << index;
		  if (name.compare(s.str()) == 0){
			  oExpr = ConstantExpr::alloc(llvm::APFloat(value));
		  } else
			  oExpr = iExpr;
		  return;
	    }
	}
	ref<Expr> exp1, exp2, cond;
	switch( iExpr->getKind()){

		default:
			oExpr = iExpr;
			break;
		case Expr::Eq:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = EqExpr::create(exp1,exp2);
			break;
		case Expr::Ne:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = NeExpr::create(exp1,exp2);
			break;
		case Expr::Slt:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = SltExpr::create(exp1,exp2);
			break;
		case Expr::Ult:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = UltExpr::create(exp1,exp2);
			break;
		case Expr::Sle:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = SleExpr::create(exp1,exp2);
			break;
		case Expr::Ule:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = UleExpr::create(exp1,exp2);
			break;
		case Expr::Sgt:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = SgtExpr::create(exp1,exp2);
			break;
		case Expr::Ugt:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = UgtExpr::create(exp1,exp2);
			break;
		case Expr::Uge:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = UgeExpr::create(exp1,exp2);
			break;
		case Expr::Sge:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = SgeExpr::create(exp1,exp2);
			break;

		/// Bitwise Expressions
		case Expr::Not:
			concretize(iExpr->getKid(0),index,value,exp1);
			oExpr = NotExpr::create(exp1);
			break;
		case Expr::And:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = AndExpr::create(exp1,exp2);
			break;
		case Expr::Or:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = OrExpr::create(exp1,exp2);
			break;
		case Expr::Xor:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = XorExpr::create(exp1,exp2);
			break;

		/// Arithmetic Expressions
		case Expr::Add:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = AddExpr::create(exp1,exp2);
			break;
		case Expr::Sub:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = SubExpr::create(exp1,exp2);
			break;
		case Expr::Mul:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = MulExpr::create(exp1,exp2);
			break;
		case Expr::UDiv:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = UDivExpr::create(exp1,exp2);
			break;
		case Expr::SDiv:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = SDivExpr::create(exp1,exp2);
			break;
		case Expr::Div:
			concretize(iExpr->getKid(0),index,value,exp1);
			concretize(iExpr->getKid(1),index,value,exp2);
			oExpr = DivExpr::create(exp1,exp2);
			break;

		case Expr::Select:
			concretize(iExpr->getKid(0),index,value,cond);
			concretize(iExpr->getKid(1),index,value,exp1);
			concretize(iExpr->getKid(2),index,value,exp2);
			oExpr = SelectExpr::create(cond,exp1,exp2);
			break;

		/*case Expr::ZExt:
		case Expr::SExt: */
		}
}

// Concretize constraints with a concrete value for an index
// 1 if succeed
// 0 if find a false constraint
// -1 if fail to concretize
int AriadneSolver::concretize(
	ConstraintManager& constraints, int index, double value,
	ConstraintManager& oConstraints
) {
	constraints.concretize();
	for (ConstraintManager::const_iterator it = constraints.begin(),
		ie = constraints.end(); it != ie; it++) {
		ref<Expr> expr = *it;
		ref<Expr> exp;
		concretize(expr, index, value, exp);
		if (ConstantExpr *C = dyn_cast<ConstantExpr> (exp)){
			if (C->isFalse()) {
				return 0;
			}
			if (C->isTrue()) {
				continue;
			}
		} else
			oConstraints.addConstraint(exp);
	}
	/// Update the number of variables in new constraints
	oConstraints.findNumVars(varNum);
	return 1;
}

// Concretize constraints with one selection strategy
// 1 if the obtained constraints are sat
// 0 if find a false constraint
// -1 if fail to concretize
int AriadneSolver::concretizeAll (
	ConstraintManager& constraints,
	const std::vector<const Array*> &objects,
	std::vector< std::vector<unsigned char> > &values,
	bool printValues
) {
	constraints.concretize();
	int success = 1; // Assuming that we can get sat constraints
	ConstraintManager oConstraints;
	ConstraintManager originalConstraints = constraints;
	int concretizingNum = 0;
	bool found = false; // check if we can find sat constraints
	double max = 100000;
	// FIXME: might need a better strategy to randomly generate inputs
	// Add interval [-1.0, 1.0] as initial range to examine
	double start = -1.0;
	double end = 1.0;
	while ( concretizingNum < numOfIntervals && !found) {
		// First concretize all input variables
		// store the list of concrete values for input vars
		double *inputValues = new double[inputNum];
		for (int i = 1; i <= inputNum; i++) {
			ConstraintManager oConstraints;
			double x = getRandomValue(start, end);
			inputValues[i-1] = x;
			//Store inputs as rational format
			std::string value = doubleToRational(x);
			insertAssignment(i, value);
			int check = concretize(constraints, i, x, oConstraints);
			// fail to concretize
			if (check == -1) {
				klee_warning("Fail to concretize constraints");
				return -1;
			}
			// detect a false constraint
			else if (check == 0) {
				success = 0;
				break;
			}
			constraints = oConstraints;
			success = 1;
		}
		if (success == 0) {
			constraints = originalConstraints;
			start = end;
			end = ((concretizingNum + 1) * max) / numOfIntervals;
			concretizingNum++;
			continue;
		}
		// Evaluate the temporary variables based on the concrete values
		// of input variables
		std::string inputList;
		std::stringstream inputListStream;
		for (int i = 1; i < inputNum; i++) {
			inputListStream << "arr";
			inputListStream << i;
			inputListStream << ",";
		}
		inputListStream << "arr";
		inputListStream << inputNum;
		inputList = inputListStream.str();

		// For temporary variables, we use FunctionParser and symbolic
		// variable table to compute the values.

		for (int i = inputNum + 1; i <= varNum; i++){
			FunctionParser fp;
			std::string varName;
			std::stringstream varNameStream;
			varNameStream << "arr";
			varNameStream << i;
			varName = varNameStream.str();
			std::string expr = varTable[varName].second->toString();
			int function = varTable[varName].first;
			fp.Parse(expr,inputList);
			double expVal = fp.Eval(inputValues);
			double value; // concrete value of temporary variable
			switch (function) {
				case 1:
					/// Handle the invalid values
					if (expVal < 0) {
						klee_warning("Fail to concretize constraints");
						return -1;
					}
					value = sqrt(expVal);
					break;
				case 2:
					value = exp(expVal);
					break;
				case 3:
					/// Handle the invalid values
					if (expVal <= 0) {
						klee_warning("Fail to concretize constraints");
						return -1;
					}
					value = log (expVal);
					break;
				case 4:
					value = sin (expVal);
					break;
				case 5:
					value = cos (expVal);
					break;
				//FIXME: need to handle pow
				default:
					value = getRandomValue();
					break;
			}
			// Handle inf values
			if (value > DBL_MAX)
				value = DBL_MAX;
			if (value < -DBL_MAX)
				value = -DBL_MAX;
			// Store inputs as rational
			std::string vString = doubleToRational(value);
			insertAssignment(i, vString);
			int check = concretize(constraints, i, value, oConstraints);
			// fail to concretize
			if (check == -1) {
				klee_warning("Fail to concretize constraints");
				return -1;
			}
			// detect a false constraint
			else if (check == 0) {
				success = 0;
				break;
			}
			constraints = oConstraints;
		}
		if (success == 0) {
			// restore the constraints
			constraints = originalConstraints;
			start = end;
			end = ((concretizingNum +1) * max) / numOfIntervals;
			concretizingNum ++;
			continue;
		}
		if (success == 1) {
			if (checkTestCase)
				success = checkTestCases(objects, values);
			if (success == 1 && printValues)
				// Print the result, set found
				printTestCases(objects, values);
			found = true;
			return success;
		} else {
			// restore the constraints
			constraints = originalConstraints;
			start = end;
			end = ((concretizingNum +1) * max) / numOfIntervals;
			concretizingNum ++;
			continue;
		}
	}
	return 0;
}

/// Assign a concrete value for a variable with given index
void AriadneSolver::insertAssignment (
	int index, std::string value
) {
	std::set<std::string>::iterator it;
	std::ostringstream s;
	s << "arr" << index;
	it = namesList.find(s.str());
	if (it != namesList.end()) {
		assignment[s.str()] = value;
		concretizing[s.str()] = true;
	} else {
		namesList.insert(s.str());
		assignment.insert (
			std::pair<std::string, std::string>(s.str(),value)
		);
		concretizing.insert(
			std::pair<std::string, bool>(s.str(),true)
		);
	}
}

std::string AriadneSolver::doubleToRational( double x ) {
	mpq_t val;
	mpq_init(val);
	mpq_set_d(val, x);
	char* cVal = mpq_get_str(NULL, 10, val);
	std::string value(cVal);
	return value;
}

int AriadneSolver::getDegree(ref<Expr> exp, int index) {
	int degree = 0;
	ref<Expr> oExp;
	Poly p,q,r;
	Poly::CompareType type;
	for(int j = 1; j <=varNum; j++){
		if(j != index){
			concretize( exp, j, 1.0, oExp );
			exp = oExp;
		}
	}
	exp->toPolyFraction(p, q, type);
	r = p.multiply(q);
	degree = r.getDegree();
	return degree;
}

int max(int val1, int val2) {
	if (val1 > val2)
		return val1;
	else
		return val2;
}

int AriadneSolver::getDegree(ref<Expr> exp) {
	if (exp.isNull())
		return 0;

	//Handle Select Expr
	if (exp->getKind() == Expr::Constant) {
		return 0;
	} else {
		if (exp->getKind() == Expr::Concat) {
			return 1;
		}
	}
	int degree = 0, d1, d2, d3;
	switch (exp->getKind()) {
	default:
		break;
	case Expr::Eq:
	case Expr::Ne:
	case Expr::Slt:
	case Expr::Ult:
	case Expr::Sle:
	case Expr::Ule:
	case Expr::Sgt:
	case Expr::Ugt:
	case Expr::Uge:
	case Expr::Sge:
	case Expr::And:
	case Expr::Or:
	case Expr::Xor:
	case Expr::Add:
	case Expr::Sub:
		d1 = getDegree(exp->getKid(0));
		d2 = getDegree(exp->getKid(1));
		degree = max(d1, d2);
		break;

	//Bit arithmetic
	case Expr::Not:
		degree = getDegree(exp->getKid(0));
		break;

	case Expr::Mul:
	case Expr::UDiv:
	case Expr::SDiv:
	case Expr::Div:
		d1 = getDegree(exp->getKid(0));
		d2 = getDegree(exp->getKid(1));
		degree = d1 + d2;
		break;
	case Expr::Select:
		d1 = getDegree(exp->getKid(0));
		d2 = getDegree(exp->getKid(1));
		d3 = getDegree(exp->getKid(2));
		degree = max(d1, d2);
		degree = max(degree, d3);
		break;
	/*case Expr::ZExt:
	 case Expr::SExt: */
	}
	return degree;
}

/// Check if a given expression contains division operation or not.
bool AriadneSolver::containDivOp(ref<Expr> exp) {
	if (exp.isNull())
		return false;

	if (exp->getKind() == Expr::Constant) {
		return false;
	} else {
		if (exp->getKind() == Expr::Concat) {
			return false;
		}
	}

	/// FIXME: might need to handle other types of exceptions
	switch (exp->getKind()) {
	default:
		break;
	case Expr::Eq:
	case Expr::Ne:
	case Expr::Slt:
	case Expr::Ult:
	case Expr::Sle:
	case Expr::Ule:
	case Expr::Sgt:
	case Expr::Ugt:
	case Expr::Uge:
	case Expr::Sge:
	case Expr::And:
	case Expr::Or:
	case Expr::Xor:
	case Expr::Add:
	case Expr::Sub:
	case Expr::Mul:
		if (containDivOp(exp->getKid(0)))
			return true;
		if (containDivOp(exp->getKid(1)))
			return true;
		break;

		//Bit arithmetic
	case Expr::Not:
		if (containDivOp(exp->getKid(0)))
			return true;
		break;

	case Expr::UDiv:
	case Expr::SDiv:
	case Expr::Div:
		return true;
	case Expr::Select:
		if (containDivOp(exp->getKid(0)))
			return true;
		if (containDivOp(exp->getKid(1)))
			return true;
		if (containDivOp(exp->getKid(2)))
			return true;
		break;
		/*case Expr::ZExt:
		 case Expr::SExt: */
	}
	return false;
}

/// Check if a given expression is linear
bool AriadneSolver::isLinear(ref<Expr> exp) {
	if (getDegree(exp) <= 1 && !containDivOp(exp))
		return true;
	else
		return false;
}

/// Check if given constraints are linear
bool AriadneSolver::isLinear(ConstraintManager constraints) {
	for (
		ConstraintManager::const_iterator it = constraints.begin(),
		ie = constraints.end(); it != ie; it++
	) {
		ref<Expr> expr = *it;
		if (!isLinear(expr))
			return false;
	}
	return true;
}

/// Get the maximum degree of variables in given constraints
int AriadneSolver::getMaxDegree(const ConstraintManager& constraints) {
	int maxDegree = 0;
	for (int i = 1; i<= inputNum; i++){
		for (
			ConstraintManager::const_iterator it = constraints.begin(),
			ie = constraints.end(); it != ie; it++
		) {
			ref<Expr> expr = *it;
			int num = getDegree(expr,i);
			if (num > maxDegree){
				maxDegree = num;
			}
		}
	}
	return maxDegree;
}

/// Based on the strategy to concretize inputs, return the index of only left symbolic
/// variable
int AriadneSolver::getSymbolicIndex(const ConstraintManager& constraints){
	int index = 1;
	int maxDegree = 0;
	int minDegree = 1000000; // set default infinity degree
	switch (select) {
		case Random:
			index = getRandInt() % inputNum + 1;
			break;
		case Descending:
			for (int i = 1; i<= inputNum; i++){
				for (ConstraintManager::const_iterator it = constraints.begin(),
						ie = constraints.end(); it != ie; it++){
					ref<Expr> expr = *it;
					int num = getDegree(expr,i);
					if (num > maxDegree){
						maxDegree = num;
						index = i;
					}
				}
			}
			break;
		case Ascending:
			for (int i = 1; i<= inputNum; i++){
				for (ConstraintManager::const_iterator it = constraints.begin(),
						ie = constraints.end(); it != ie; it++){
					ref<Expr> expr = *it;
					int num = getDegree(expr,i);
					if (num < minDegree){
						minDegree = num;
						index = i;
					}
				}
			}
			break;
		default:
			break;

	}
	return index;
}

/// Concretize constraints with given remaining index
int AriadneSolver::concretizeWithRemainingIndex (
	ConstraintManager& constraints, const int index
) {
	ConstraintManager oConstraints;
	std::ostringstream sIndex;
	sIndex << "arr" << index;
	for (int i = 1; i <= inputNum; i++) {
		if (i != index) {
			double x = getRandomValue();
			// Change the double value to rational value
			std::string value = doubleToRational(x);
			insertAssignment(i, value);
			int check = concretize(constraints, i, x, oConstraints);
			// fail to concretize
			if (check == -1) {
				klee_warning("Fail to concretize constraints");
				return -1;
			}
			// detect a false constraint
			else if (check == 0) {
				return 0;
			}
			constraints = oConstraints;
		}
	}
	return 1;
}

/*
 * Concretize constraints
 * 1 if success
 * 0 if can not get sat constraints
 * -1 if fail to concretize
 */
int AriadneSolver::concretize( ConstraintManager& constraints, int &index) {
	ConstraintManager oConstraints;
	// Do exhaustive concretization until success
	if (select == Exhaustive) {
		for (int i = 1; i <= inputNum; i++) {
			ConstraintManager tmpConstraints = constraints;
			int success = concretizeWithRemainingIndex(tmpConstraints, i);
			if (success == 1) {
				index = i;
				constraints = tmpConstraints;
				return 1;
			}
		}
		return 0;
	}
	/// Other cases, choose the index for the remaining symbolic variable and
	/// concretize
	else {
		index = getSymbolicIndex(constraints);
		return concretizeWithRemainingIndex(constraints, index);
	}
}

/*
 *  Solve linear, single-variable path conditions to find
 *  exception-triggering floating-point inputs
 */
//0 -> unsat
//1 -> sat
//-1 -> unsolvable
int AriadneSolver::solveLinearPC (
	ConstraintManager& constraints,
	const std::vector<const Array*> &objects, int index
) {
  
    SMT_result res;
    res = SMT_check_and_get_model( constraints);
	if (res == ST_UNDEF)
	  return 0;
	else if (res == ST_FALSE) {
	  klee_warning("Failed to construct constraints");
	  return -1;
	}

	std::stringstream s;
	s << "arr" << index;
	std::string name = s.str();
	const Array* array = new Array( name, Expr::Int64 );

	while (res == SMT_L_TRUE) {
		/// Evaluate the model, get a satisfying assignment
		mpq_t mpqValue;
		mpq_init( mpqValue );
		std::string valueStr = assignment[array->name];
		mpq_set_str( mpqValue, valueStr.c_str(), 10 );
		/// Round the satisfying rational to floating point value,
		/// consider as middle value in the satisfied interval
		double mid = mpq_get_d( mpqValue );
		ref<Expr> midVal = cast<Expr> (
			ConstantExpr::alloc(llvm::APFloat(mid))
		);
		ref<Expr> var = Expr::createTempRead( array, Expr::Int64 );
		ref<Expr> isMid = cast<Expr> (EqExpr::create( var, midVal ));

		ConstraintManager tmpConstraints = constraints;
		/// Check if the middle floating point value satisfies the constraints or not
		tmpConstraints.addConstraint( isMid );

		res = SMT_check_and_get_model( tmpConstraints );
		if (res == SMT_L_TRUE) {
			return 1;
		} else {
			/// Consider the next floating point value in the interval
			double post = nextafter( mid, DBL_MAX );
			ref<Expr> postVal = cast<Expr> (
				ConstantExpr::alloc(llvm::APFloat(post))
			);
			ref<Expr> isPost = cast<Expr> (EqExpr::create(var, postVal));
			tmpConstraints = constraints; // Reset to original constraints
			tmpConstraints.addConstraint( isPost );
			res = SMT_check_and_get_model( tmpConstraints );
			
			if (res == SMT_L_TRUE) {
				return 1;
			} else {
				/// Consider the previous floating point value in the interval
				double pre = nextafter( mid, -DBL_MAX );
				ref<Expr> preVal = cast<Expr> (
					ConstantExpr::alloc( llvm::APFloat(pre) )
				);
				ref<Expr> isPre = cast<Expr> ( EqExpr::create(var, preVal) );
				tmpConstraints = constraints;
				tmpConstraints.addConstraint( isPre );
				res = SMT_check_and_get_model( tmpConstraints );
				if (res == SMT_L_TRUE) {
					return 1;
				} else {
					/*
					 * Ignore end cases to avoid infinitive loop
					 */
					if (mid == DBL_MAX || mid == -DBL_MAX)
						return 0;
					/// In case three consecutive values in the interval do not
					/// satisfy the constraints, add additional condition to
					/// rule-out the interval
					ref<Expr> ltPre = cast<Expr> (SltExpr::create(var, preVal));
					ref<Expr> gtPost = cast<Expr> (SgtExpr::create(var, postVal));
					ref<Expr> condition = cast<Expr> (OrExpr::create(ltPre, gtPost));
					constraints.addConstraint( condition );
					res = SMT_check_and_get_model( constraints );
				}
			}
		}
	}
	if (res == SMT_L_FALSE) {
		return 0;
	} else {
		return -1;
	}
}

/*
 *  Solve arbitrary path conditions with single variable to find
 *  exception-triggering floating-point inputs
 */
// -1 -> unsolvable
// 0 -> unsat
// 1 -> sat
int AriadneSolver::solveSinglePC (
	ConstraintManager& constraints,
	const std::vector<const Array*> &objects, int index,
	std::vector< std::vector<unsigned char> > &values, bool timed
) {
	assert(!objects.empty() && "The list of symbolic objects is empty");
	ConstraintManager orgConstraints = constraints;
	sys::TimeValue now(0,0),user(0,0),delta(0,0),sys(0,0);
	sys::Process::GetTimeUsage(now,user,sys);

	/// Reset concretizing tables
	resetConcretizingTable(constraints);

	// Pre-processing
	ConstraintManager oConstraints;
	std::stringstream s;
	s << "arr" << index;
	std::string name = s.str();
	Array* array = new Array(name, Expr::Int64);
	ref<Expr> var = Expr::createTempRead(array, Expr::Int64);

	bool isLinearConstraints = isLinear(constraints);

	bool linearizable = linearize(constraints, var, oConstraints);

	if(!linearizable) {
		klee_warning("Fail to linearize constraints");
		return -1;
	}
	int success = solveLinearPC(oConstraints, objects, index);
	sys::Process::GetTimeUsage(delta, user, sys);
	delta -= now;
	if (success == 1) {
		if (checkTestCase)
			success = checkTestCases(objects,values);
		if (success == 1) {
			if (!isLinearConstraints && !isMultiVar) {
				poly_queries++;
				poly_single_sat_queries++;
				poly_sat_queries++;
				stats::polyTime += delta.usec();
			}
			else if ( !isMultiVar ) {
				z3_queries++;
				z3_sat_queries++;
				stats::z3Time += delta.usec();
			}
			printTestCases(objects, values);
		} else {
			if (!isLinearConstraints && !isMultiVar) {
				poly_queries++;
				poly_single_unsat_queries++;
				poly_unsat_queries++;
				stats::polyTime += delta.usec();
			}
			else if ( !isMultiVar ) {
				z3_queries++;
				z3_unsat_queries++;
				stats::z3Time += delta.usec();
			}
		}
	} else if (success == 0) {
		if (!isLinearConstraints && !isMultiVar) {
			poly_queries++;
			poly_single_unsat_queries++;
			poly_unsat_queries++;
			stats::polyTime += delta.usec();
		}
		else if ( !isMultiVar ) {
			z3_queries++;
			z3_unsat_queries++;
			stats::z3Time += delta.usec();
		}
	}
	return success;
}

/// Reset values of concretizing tables, all variables are considered as non-concretizing
void AriadneSolver::resetConcretizingTable(ConstraintManager constraints) {
	for (int i = 1; i<=varNum; i++ ) {
		std::string varName;
		std::stringstream varNameStream;
		varNameStream << "arr";
		varNameStream << i;
		varName = varNameStream.str();
		if (find(constraints,varName))
			concretizing[varName] = false;
	}
}

int AriadneSolver::findFPSolutions(
		ConstraintManager& constraints,
		const std::vector<const Array*> &objects,
		std::vector< std::vector<unsigned char> > &values
) {
	for (std::vector<const Array*>::const_iterator it = objects.begin(), ie =
			objects.end(); it != ie; ++it) {
		const Array *array = *it;
		mpq_t mpqValue;
		mpq_init(mpqValue);
		std::string strValue = assignment[array->name];
		mpq_set_str(mpqValue, strValue.c_str(), 10);
		double mid = mpq_get_d(mpqValue);
		ref<Expr> midVal = cast<Expr> (ConstantExpr::alloc(llvm::APFloat(mid)));
		ref<Expr> var = Expr::createTempRead(array, Expr::Int64);
		ref<Expr> isMid = cast<Expr> (EqExpr::create(var, midVal));
		ConstraintManager tmpConstraints = constraints;
		tmpConstraints.addConstraint(isMid);

		SMT_result res = SMT_check_and_get_model(tmpConstraints);
		if (res == SMT_L_TRUE) {
			constraints = tmpConstraints;
			continue;
		}
		double pre = nextafter(mid, -DBL_MAX);
		ref<Expr> preVal = cast<Expr> (ConstantExpr::alloc(llvm::APFloat(pre)));
		ref<Expr> isPre = cast<Expr> (EqExpr::create(var, preVal));
		tmpConstraints = constraints;
		tmpConstraints.addConstraint(isPre);

		res = SMT_check_and_get_model(tmpConstraints);
		if (res == SMT_L_TRUE) {
			constraints = tmpConstraints;
			continue;
		}

		double post = nextafter(mid, DBL_MAX);
		ref<Expr> postVal = cast<Expr> (
				ConstantExpr::alloc(llvm::APFloat(post)));
		ref<Expr> isPost = cast<Expr> (EqExpr::create(var, postVal));
		tmpConstraints = constraints;
		tmpConstraints.addConstraint(isPost);
		res = SMT_check_and_get_model(tmpConstraints);
		if (res == SMT_L_TRUE) {
			continue;
		} else if (res == SMT_L_FALSE)
			return 0;
		else
			return -1;
	}
	return 1;
}

/*
 *  Solve arbitrary path conditions with multiple variables to find
 *  exception-triggering floating-point inputs
 */
// -1 -> unsolvable
// 0 -> unsat
// 1 -> sat
int AriadneSolver::solveMultiplePC (
	ConstraintManager& constraints,
	const std::vector<const Array*> &objects,
	std::vector< std::vector<unsigned char> > &values
) {
	assert(!objects.empty() && "The list of symbolic objects is empty");
	ConstraintManager orgConstraints = constraints;
	sys::TimeValue now(0,0),user(0,0),delta(0,0),sys(0,0);
	sys::Process::GetTimeUsage(now,user,sys);
	int success = 0;

	/// Reset concretizing tables
	resetConcretizingTable(constraints);

	// If there are dependent variables in the constraints,
	// immediately concretize all input variables.
	if (hasDependentVars(constraints)) {
	  sys::Process::GetTimeUsage(delta,user,sys);
	  delta -= now;
	  success = concretizeAll(constraints, objects, values);
	  if (success == 1 ) {
		  poly_queries++;
		  poly_sat_queries++;
		  poly_multiple_sat_queries++;
		  stats::polyTime += delta.usec();
	  }
	  else if (success == 0) {
		  poly_queries++;
		  poly_unsat_queries++;
		  poly_multiple_unsat_queries++;
		  stats::polyTime += delta.usec();
	  }
	  return success;
	}
	SMT_result res; 
	res = SMT_check_and_get_model(constraints);
	if (res == ST_UNDEF) {
		poly_queries++;
		poly_unsat_queries++;
		poly_multiple_unsat_queries++;
		return success;
	}
	else if (res == ST_FALSE) {
		klee_warning("Fail to construct constraints");
		success = -1;
		return success;
	}
	sys::Process::GetTimeUsage(delta,user,sys);
	delta -= now;
	switch (res) {
		case SMT_L_TRUE:
			stats::z3Time += delta.usec();
			success = findFPSolutions(constraints, objects, values);
			if (success < 0) {
				return success;
			}
			else if (success == 0) {
				z3_queries++;
				z3_unsat_queries++;
				return success;
			}
			if (checkTestCase)
				success = checkTestCases(objects, values);
			if (success == 1) {
				z3_queries++;
				z3_sat_queries++;
				printTestCases(objects, values);
			} else {
				z3_queries++;
				z3_unsat_queries++;
			}
			break;

		case SMT_L_FALSE:
			z3_queries++;
			z3_unsat_queries++;
			stats::z3Time += delta.usec();
			success = 0;
			break;

		default:
			ConstraintManager oConstraints;
			int index = 1;
			int check = -1;
			ConstraintManager origConstraints = constraints;
			for (int counter = 1; counter <= maxConcretizations; counter++) {
				check = concretize(constraints, index);
				if (check == 1)
					break;
				if ( counter < maxConcretizations )
					constraints = origConstraints;
			}

			if (check == -1) {
				klee_warning("Failed to concretize constraints");
				failedConcretization++;
				return -1;
			} else if (check == 0) {
				failedConcretization++;
				poly_queries++;
				poly_unsat_queries++;
				poly_multiple_unsat_queries++;
				sys::Process::GetTimeUsage(delta, user, sys);
				delta -= now;
				stats::polyTime += delta.usec();
				return 0;
			}
			success = solveSinglePC(constraints, objects, index, values);
			if (success > 0)
				successfulConcretization++;
			else
				failedConcretization++;

			if (success >= 0) {
				poly_queries++;
				if ( success > 0 ) {
					poly_multiple_sat_queries++;
					poly_sat_queries++;
				} else {
					poly_multiple_unsat_queries++;
					poly_unsat_queries++;
				}
				sys::Process::GetTimeUsage(delta, user, sys);
				delta -= now;
				stats::polyTime += delta.usec();
			}
			break;
	  }
	  return success;
}

/***/

AriadneSolverImpl::AriadneSolverImpl(AriadneSolver *_solver)
  : solver(_solver),
    polyBuilder(new PolyBuilder()),
    smtBuilder(new Z3Builder()),
    timeout(0.0)
{
}

AriadneSolverImpl::~AriadneSolverImpl(){
}

char *AriadneSolverImpl::getConstraintLog(const Query &query) {
  char *buffer = NULL;
  return buffer;
}

bool AriadneSolverImpl::computeTruth(const Query& query,
                                 bool &isValid) {
  std::vector<const Array*> objects;
  std::vector< std::vector<unsigned char> > values;
  bool hasSolution;

  if (!computeInitialValues(query, objects, values, hasSolution))
    return false;

  isValid = !hasSolution;
  return true;
}

bool AriadneSolverImpl::computeValue(const Query& query,
									 ref<Expr> &result) {
  std::vector<const Array*> objects;
  std::vector< std::vector<unsigned char> > values;
  bool hasSolution;

  // Find the object used in the expression, and compute an assignment
  // for them.
  findSymbolicObjects(query.expr, objects);
  if (!computeInitialValues(query.withFalse(), objects, values, hasSolution))
    return false;
  assert(hasSolution && "state has invalid constraint set");

  // Evaluate the expression with the computed assignment.
  Assignment a(objects, values);
  result = a.evaluate(query.expr);

  return true;
}

bool AriadneSolverImpl::computeInitialValues(
	const Query &query, const std::vector<const Array*> &objects,
	std::vector<std::vector<unsigned char> > &values, bool &hasSolution
) {
	if (objects.empty()) {
		hasSolution = true;
		return true;
	}
	ConstraintManager constraints = query.constraints;
	ref<Expr> neg = Expr::createIsZero(query.expr);
	if (ConstantExpr *CE = dyn_cast<ConstantExpr>(neg)) {
	    if (CE->isFalse()) {
	      hasSolution = false;
	      return true;
	    }
	} else {
		constraints.addConstraint(neg);
	}

	constraints.findNumVars(solver->getNumVar());
	int success;
	if (constraints.getNumVars() > 1) {
		solver->setMultiVar(true);
		success = solver->solveMultiplePC(constraints, objects, values);
	}
	else {
		solver->setMultiVar(false);
		int index = solver->getFirstVarIndex(constraints);
		success = solver->solveSinglePC(constraints, objects, index, values);
	}

	if (success > 0) {
		hasSolution = true;
		return true;
	} else if ( success == 0) {
		hasSolution = false;
		return true;
	}
	else {
		hasSolution = false;
		return false;
	}
}

ConstraintManager AriadneSolver::queryToConstraints( const Query query) {
	ConstraintManager constraints = ConstraintManager(query.constraints);
	constraints.addConstraint(query.expr);
	return constraints;
}

SMT_result AriadneSolver::SMT_check( ConstraintManager constraints) {
  return static_cast<AriadneSolverImpl*>(impl)->smtBuilder->SMT_check(constraints, this);
}

SMT_result AriadneSolver::SMT_check_and_get_model( ConstraintManager constraints ) {
  return static_cast<AriadneSolverImpl*>(impl)->smtBuilder->SMT_check_and_get_model(constraints, this);
}
