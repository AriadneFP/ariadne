/*
 * CexCachingSolver.h
 *
 *  Created on: Sep 29, 2011
 *      Author: thanh
 */

#ifndef CEXCACHINGSOLVER_H_
#define CEXCACHINGSOLVER_H_

#include "klee/Solver.h"
#include "klee/Constraints.h"
#include "klee/Expr.h"
#include "klee/SolverImpl.h"


namespace klee {
///

typedef std::set< ref<Expr> > KeyType;

struct AssignmentLessThan {
  bool operator()(const Assignment *a, const Assignment *b) {
    return a->bindings < b->bindings;
  }
};

///

struct NullAssignment {
  bool operator()(Assignment *a) const { return !a; }
};

struct NonNullAssignment {
  bool operator()(Assignment *a) const { return a!=0; }
};

struct NullOrSatisfyingAssignment {
  KeyType &key;

  NullOrSatisfyingAssignment(KeyType &_key) : key(_key) {}

  bool operator()(Assignment *a) const {
    return !a || a->satisfies(key.begin(), key.end());
  }
};


class CexCachingSolver : public SolverImpl {
  typedef std::set<Assignment*, AssignmentLessThan> assignmentsTable_ty;

  Solver *solver;

  MapOfSets<ref<Expr>, Assignment*> cache;
  // memo table
  assignmentsTable_ty assignmentsTable;

  bool searchForAssignment(KeyType &key,
                           Assignment *&result);

  bool lookupAssignment(const Query& query, KeyType &key, Assignment *&result);

  bool lookupAssignment(const Query& query, Assignment *&result) {
    KeyType key;
    return lookupAssignment(query, key, result);
  }

  bool getAssignment(const Query& query, Assignment *&result);

public:
  CexCachingSolver(Solver *_solver) : solver(_solver) {}
  ~CexCachingSolver();

  bool computeTruth(const Query&, bool &isValid);
  bool computeValidity(const Query&, Solver::Validity &result);
  bool computeValue(const Query&, ref<Expr> &result);
  bool computeInitialValues(const Query&,
                            const std::vector<const Array*> &objects,
                            std::vector< std::vector<unsigned char> > &values,
                            bool &hasSolution);
};
}

#endif /* CEXCACHINGSOLVER_H_ */
