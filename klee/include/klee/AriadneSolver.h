/*
 * AriadneSolver.h
 *
 *  Created on: Apr 5, 2011
 *      Author: thanh
 */


#ifndef ARIADNESOLVER_H_
#define ARIADNESOLVER_H_

#include "klee/Constraints.h"
#include "klee/Expr.h"
#include "klee/util/ExprPPrinter.h"
#include "klee/Solver.h"
#include "klee/SolverImpl.h"
#include "klee/Poly.h"
#include "klee/PolyBuilder.h"
#include <string>

namespace klee {

    enum SMT_result {
    	SMT_L_FALSE = -1,
    	SMT_L_UNDEF = 0,
    	SMT_L_TRUE = 1,
    	ST_FALSE = 2,
    	ST_UNDEF = 3,
    	ST_TRUE = 4
    };

  	// AriadneSolver - A complete solver based on Z3 and Poly Solver
	class AriadneSolver : public Solver {

	  /// The type to select a variable as the remaining symbolic variable
	  enum SelectionType {
			Random,
			Ascending,
			Descending,
			Exhaustive
	  };

	  SelectionType select;

	  Z3_config config;

	  /// Concretization stat fields
	  int maxConcretizations;

	  int queryNum;

	  int successfulConcretization;

	  int failedConcretization;

	  // The path info
	  unsigned satPaths;
	  unsigned unsatPaths;
	  unsigned unknownPaths;

	  void printConstraints(
		  ConstraintManager constraints, bool concretized, int success
	  );

	  //Handling AST of Z3 native format
	  //Store a list of free variables in the set of ReadExprs
	  std::set<const ReadExpr*> usedExprs;
	  //A map to store each ReadExpr with an AST
	  typedef std::map <std::string, Z3_ast > bindings_ast;
	  bindings_ast varList;

	  bool find(ref<Expr> e, const std::string name);

	  ///get the first variable index of an expression
	  int getFirstIndex(ref<Expr> e);

	  ConstantExpr getRandomConstExpr();
	  const ReadExpr* extractReadExpr(ref<Expr> e);

	  int numOfIntervals; // number of intervals to concretize random values

	  /// Approximate timeout for each solver query ( milliseconds)
	  unsigned int timeOut;

	  std::string timeoutStr;

	  /// Number of queries that Z3 can solve
	  unsigned int z3_queries;
	  /// Number of sat queries that Z3 can solve
	  unsigned int z3_sat_queries;
	  /// Number of unsat queries that Z3 can solve
	  unsigned int z3_unsat_queries;
	  /// Number of sat queries that require Poly solver to solve
	  unsigned int poly_sat_queries;
	  /// Number of sat single variable queries that require Poly solver to solve
	  unsigned int poly_single_sat_queries;
	  /// Number of sat multiple variable queries that require Poly solver to solve
	  unsigned int poly_multiple_sat_queries;
	  /// Number of unsat queries that require Poly solver to solve
	  unsigned int poly_unsat_queries;
	  /// Number of unsat single variable queries that require Poly solver to solve
	  unsigned int poly_single_unsat_queries;
	  /// Number of unsat multiple variable queries that require Poly solver to solve
	  unsigned int poly_multiple_unsat_queries;
	  /// Number of queries required Poly solver to solve
	  unsigned int poly_queries;

	  /// Check if the query is multivariate or single variable
	  /// This variable help avoid the case multi count the number solved queries
	  bool isMultiVar;

	  // Variable Table
	  // This table will match the temporary variable name, the function index
	  // and the expression within the function
	  // It has the format like following
	  // arr1    1    "arr1*arr1 +1" ->sqrt (arr1*arr1+1)
	  std::map<std::string, std::pair<int,ref<Expr> > > varTable;

	  /* Total number of variables including dependent variables*/
	  int varNum;
	  int inputNum; //Number of input variables

	  void declare(ref<Expr> e);

	  /// Check if an expression is boolean
	  bool isbool(const ref<Expr> exp);

	  bool containDivOp(const ref<Expr> exp);

	  std::string doubleToRational( double x );

	  void insertAssignment( int index, std::string value );

	  bool checkTestCase;

	  bool print; // check if we need to print test cases or not

	  void resetConcretizingTable(ConstraintManager constraints);

	  /// After we find rational solutions, we check if they are valid floating
	  /// point solutions and iteratively query the Solver until we find valid ones
	  /// return 1 if we can find floating point solutions, 0 if we can not
	  /// -1 if the solver failed to solve the constraints.
	  int findFPSolutions(
	  	  ConstraintManager& constraints,
	  	  const std::vector<const Array*> &objects,
	  	  std::vector< std::vector<unsigned char> > &values
	  );



	  int getRandInt();
	  /// This is a kludge, we use this field to check if we use boost library
	  /// for random generator

	  bool useBoost;

    public:

	  /// AriadneSolver - Construct a new AriadneSolver.
	  AriadneSolver();

	  ~AriadneSolver(){}

	  
	  //A map to store each ReadExpr with a concrete values
 	  typedef std::map <std::string, std::string > bindings;
  	  bindings assignment;

	  //Store a list of names of free variables
	  std::set<std::string> namesList;

	  //Keep the list of concretized values
	  std::map <std::string, bool> concretizing;

	  /// Check if the constraints are linear
	  bool isLinear(const ref<Expr> exp);

	  bool isLinear(ConstraintManager constraints);

	  void printTestCase() { print = true;}

	  void notPrintTestCase() { print = false;}


	  enum ExceptionType {
	  		  Overflow,
	  		  Underflow,
	  		  Invalid,
	  		  DivideByZero,
	  		  NotException
	  };

	  ExceptionType exception;
	  int location;


	  int getSuccessfulConcretization() {
		  return successfulConcretization;
	  }

	  int getFailedConcretization() {
		  return failedConcretization;
	  }

	  enum ConstraintFormat {
			  ISAT,
			  SMTLIB,
			  KLEE
	  };

	  ConstraintFormat constraintType;

	  void setConstraintFormat(ConstraintFormat _constraintType) {
		  constraintType = _constraintType;
	  }

	  /// Set the original query is multi variable or not
	  void setMultiVar(bool _multiVar) {
		  isMultiVar = _multiVar;
	  }

	  int getNumofZ3_queries() {
  		  return z3_queries;
  	  }

  	  int getNumofPoly_queries() {
  		  return poly_queries;
  	  }

  	  int getNumofZ3_sat_queries() {
  		  return z3_sat_queries;
  	  }

  	  int getNumofZ3_unsat_queries() {
  		  return z3_unsat_queries;
  	  }

  	  int getNumofPoly_sat_queries() {
  		  return poly_sat_queries;
  	  }

  	  int getNumofPoly_unsat_queries() {
  		  return poly_unsat_queries;
  	  }

  	  int getNumofPoly_single_sat_queries() {
  		  return poly_single_sat_queries;
  	  }

  	  int getNumofPoly_multiple_sat_queries() {
  		  return poly_multiple_sat_queries;
  	  }

  	  int getNumofPoly_single_unsat_queries() {
  		  return poly_single_unsat_queries;
  	  }

  	  int getNumofPoly_multiple_unsat_queries() {
  		  return poly_multiple_unsat_queries;
  	  }

	  int getFirstVarIndex(ConstraintManager constraints);

	  int getFirstVarIndex(ref<Expr> expr);


	  void printTestCases (
 	 		  const std::vector<const Array*> &objects,
			  std::vector< std::vector<unsigned char> > &result
 	  );

 	  // return 0 if the test cases fail to generate correct exception.
 	  // return 1 if the test cases generates correct exception.
 	  int checkTestCases(
 	 	  const std::vector<const Array*>	&objects,
 	   	  std::vector< std::vector<unsigned char> > &values
 	  );

	  /// getConstraintLog - Return the constrain log for the given state in CVC
	  /// format
	  char *getConstraintLog(const Query&);
	  
	  /// setTimeout - Set constraint solver timeout delay to the given value;
	  /// 0 is off.
	  void setTimeout(unsigned int timeout);

	  //Get AST native format of Z3 from an Expression
	  Z3_ast getAst(Z3_context ctx, const ref<Expr> exp, Z3_sort_kind skind);

      void setNumVar(int num);

      void setVarTable(
      		std::map<std::string, std::pair<int,ref<Expr> > > varTable
      ) {
      		this->varTable = varTable;
      }

      void setInputNum(int num) {
          	inputNum = num;
      }

      int getNumVar(){ return varNum;}

      //Obtain values for free variables
      void evalModel(Z3_context ctx, Z3_model m);

      bool linearize(
      		const ConstraintManager iConstraints,
      		ref<Expr>& var, ConstraintManager& oConstraints
      );

      //Linearize an expression
      bool linearize(const ref<Expr>iExpr, ref<Expr>& var, ref<Expr>& oExpr);

      /// FIXME: Need to change these methods as private methods
      /// Find a variable with a given name in constraints.
      bool find( ConstraintManager& constraints, const std::string name );

      // Get a random value in an interval
      double getRandomValue(double start, double end);

      double getRandomValue();

      int getDegree(ref<Expr> exp, int index);

      int getDegree(ref<Expr> exp);

      int getMaxDegree(const ConstraintManager& constraints);

      // Check if there are dependent variables in constraints
      bool hasDependentVars(ConstraintManager &constraints);

      int getSymbolicIndex(const ConstraintManager& constraints);

      // Concretize an expression with a concrete value for an index
      // 1 if succeed
      // 0 if find a false constraint
      // -1 if fail to concretize
      void concretize(ref<Expr> iExpr, int index, double value, ref<Expr>& oExpr);

      // Concretize constraints with a concrete value for an index
      // 1 if succeed
      // 0 if find a false constraint
      // -1 if fail to concretize
      int concretize (
      	ConstraintManager& constraints,
      	int index, double value, ConstraintManager& oConstraints
      );

      // Concretize constraints with one selection strategy
      // Store the index of symbolic variable in index
      // 1 if can concretize to new constraints
      // 0 if find a false constraint
      // -1 if fail to concretize
      int concretize (ConstraintManager& constraints, int &index);

      /// Concretize constraints with one single index left
      int concretizeWithRemainingIndex (
    	  ConstraintManager& constraints, const int leftIndex
      );

      int concretizeAll (
    		  ConstraintManager& constraints,
      	   	  const std::vector<const Array*> &objects,
      	   	  std::vector< std::vector<unsigned char> > &values,
      	   	  bool printValues = true
      );

      /*
       *  Solve arbitrary path conditions with single variable to find
       *  exception-triggering floating-point inputs
       */
      // -1 -> unsolvable
      // 0 -> unsat
      // 1 -> sat use
      int solveSinglePC (
    		ConstraintManager& constraints,
      		const std::vector<const Array*> &objects, int index,
      		std::vector< std::vector<unsigned char> > &values, bool timed = true
      );

      /*
       *  Solve arbitrary path conditions with multiple variables to find
       *  exception-triggering floating-point inputs
       */
      // -1 -> unsolvable
      // 0 -> unsat
      // 1 -> sat, use linear solver only
      // 2 -> sat, use non-linear constraint solvers
      int solveMultiplePC (
    		ConstraintManager& constraints,
      		const std::vector<const Array*> &objects,
      		std::vector< std::vector<unsigned char> > &values
      );

      /*
       *  Solve linear, single-variable path conditions to find
       *  exception-triggering floating-point inputs
       */
      //0 -> unsat
      //1 -> sat
      //-1 -> unsolvable
      int solveLinearPC (
      		ConstraintManager& constraints,
      		const std::vector<const Array*> &objects, int index
      );

      ConstraintManager queryToConstraints( const Query query );
      
      SMT_result SMT_check( ConstraintManager constraints );
      SMT_result SMT_check_and_get_model( ConstraintManager constraints );
      
  };
	
	
	class SMTBuilder;
	class Z3Builder;

	class AriadneSolverImpl : public SolverImpl {
	public:
		/// The solver we are part of, for access to public information.
		AriadneSolver *solver;
		PolyBuilder *polyBuilder;
		SMTBuilder *smtBuilder;
		double timeout;

		AriadneSolverImpl(AriadneSolver *_solver);

		~AriadneSolverImpl();

		char *getConstraintLog(const Query&);

		void setTimeout(double _timeout) { timeout = _timeout; }

		double getTimeout() { return timeout;}

		//Linearize an expression
		bool linearize(const ref<Expr>iExpr, ref<Expr>& var, ref<Expr>& oExpr);

		bool linearizeSingleVarExpr ( const ref<Expr>iExpr, ref<Expr>& oExpr );

		bool linearize(
			const ConstraintManager iConstraints,
			ref<Expr>& var, ConstraintManager& oConstraints
		);

		bool linearizeSingleVarConstraints(
			const ConstraintManager iConstraints, ConstraintManager& oConstraints
		);

		bool computeTruth(const Query&, bool &isValid);

		bool computeValue(const Query&, ref<Expr> &result);

		bool computeInitialValues(const Query&,
		                          const std::vector<const Array*> &objects,
		                          std::vector< std::vector<unsigned char> > &values,
		                          bool &hasSolution);

	};
}

#endif /* ARIADNESOLVER_H_ */
