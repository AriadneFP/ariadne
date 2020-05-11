//===-- Constraints.h -------------------------------------------*- C++ -*-===//
//
//                     The KLEE Symbolic Virtual Machine
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef KLEE_CONSTRAINTS_H
#define KLEE_CONSTRAINTS_H

#include <sstream>

#include "klee/Expr.h"
#include "klee/util/ExprUtil.h"

// FIXME: Currently we use ConstraintManager for two things: to pass
// sets of constraints around, and to optimize constraints. We should
// move the first usage into a separate data structure
// (ConstraintSet?) which ConstraintManager could embed if it likes.
namespace klee {

class ExprVisitor;
  
class ConstraintManager {
public:
  typedef std::vector< ref<Expr> > constraints_ty;
  typedef constraints_ty::iterator iterator;
  typedef constraints_ty::const_iterator const_iterator;

  /// The logic type that constraints could be
  enum LogicType {
	  integer,
	  real,
	  mixed_separable,
	  mixed_not_separable
  };

  ConstraintManager() {
	  concretized = false;
  }

  // create from constraints with no optimization
  explicit
  ConstraintManager(const std::vector< ref<Expr> > &_constraints) :
    constraints(_constraints) {
	  concretized = false;
  }

  ConstraintManager(const ConstraintManager &cs) : constraints(cs.constraints) {
	  numVars = cs.getNumVars();
  }

  typedef std::vector< ref<Expr> >::const_iterator constraint_iterator;

  // given a constraint which is known to be valid, attempt to 
  // simplify the existing constraint set
  void simplifyForValidConstraint(ref<Expr> e);

  ref<Expr> simplifyExpr(ref<Expr> e) const;

  void addConstraint(ref<Expr> e);
  
  bool empty() const {
    return constraints.empty();
  }
  ref<Expr> back() const {
    return constraints.back();
  }
  constraint_iterator begin() const {
    return constraints.begin();
  }
  constraint_iterator end() const {
    return constraints.end();
  }
  size_t size() const {
    return constraints.size();
  }

  bool operator==(const ConstraintManager &other) const {
    return constraints == other.constraints;
  }
  
  void concretize() {
	  concretized = true;
  }

  bool isConcretized() {
	  return concretized;
  }

  unsigned int getNumVars() const {
	  return numVars;
  }

  /// Set number of variables with given maximum number of variables
  void findNumVars(int maxVars) {
	  numVars = 0;
	  for (int i = 1; i <= maxVars; i++) {
		  std::stringstream varNameStream;
		  varNameStream << "arr" << i;
		  std::string varName = varNameStream.str();
		  // find searches this object's constraints
		  if ( find( varName ) )
			  numVars++;
		  std::stringstream sqrtNameStream;
		  sqrtNameStream << "sqrtArr" << i;
		  std::string sqrtName = sqrtNameStream.str();
		  if ( find( sqrtName ) )
		  	  numVars++;
	  }
  }

  bool containSqrtVars() {
	  for (unsigned int i = 1; i <= numVars; i++) {
		  std::stringstream sqrtNameStream;
		  sqrtNameStream << "sqrtArr" << i;
		  std::string sqrtName = sqrtNameStream.str();
		  if ( find( sqrtName ) )
			  return true;
	  }
	  return false;
  }

  unsigned int getNumVars() {
	  return numVars;
  }

  /// Set the number of variables for constraints
  void setNumVars(int _numVars) {
	  numVars = _numVars;
  }

  bool find( const std::string name ) {
  	ConstraintManager::const_iterator it = this->begin(), ie = this->end();
  	for (; it != ie; ++it) {
  		if (klee::find(*it,name))
  			return true;
  	}
  	return false;
  }

  /// Check if constraints are linear or not
  bool isLinear() {
	  return linear;
  }

  void setLinear() {
	  linear = true;
  }


private:
  std::vector< ref<Expr> > constraints;

  // returns true iff the constraints were modified
  bool rewriteConstraints(ExprVisitor &visitor);

  void addConstraintInternal(ref<Expr> e);

  /// Check if constraints need to be concretized to solve by AriadneSolver
  bool concretized;

  /// The number of variables in constraints
  unsigned int numVars;

  /// Check if constrains are linear or non-linear
  bool linear;

  LogicType type;

};

}

#endif /* KLEE_CONSTRAINTS_H */
