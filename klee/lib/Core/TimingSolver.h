//===-- TimingSolver.h ------------------------------------------*- C++ -*-===//
//
//                     The KLEE Symbolic Virtual Machine
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef KLEE_TIMINGSOLVER_H
#define KLEE_TIMINGSOLVER_H

#include "klee/Expr.h"
#include "klee/Solver.h"
#include "klee/Constraints.h"

#include <vector>
#include <sstream>
#include <string>
#include <map>

/// Ariadne
#include "klee/AriadneSolver.h"

namespace klee {
  class ExecutionState;
  class Solver;
  class STPSolver;
  class AriadneSolver;

  /// TimingSolver - A simple class which wraps a solver and handles
  /// tracking the statistics that we care about.
  class TimingSolver {

  public:

	Solver *solver;
    STPSolver *stpSolver;
    bool simplifyExprs;
    AriadneSolver *ariadneSolver;

    /// TimingSolver - Construct a new timing solver.
    ///
    /// \param _simplifyExprs - Whether expressions should be
    /// simplified (via the constraint manager interface) prior to
    /// querying.
    /// Default selection strategy is random
    TimingSolver (Solver *_solver, STPSolver *_stpSolver):
        	solver(_solver), stpSolver(_stpSolver) {
        	simplifyExprs = true;
    }

    TimingSolver (Solver *_solver, STPSolver *_stpSolver, AriadneSolver *_adriadneSolver):
    	solver(_solver), stpSolver(_stpSolver), ariadneSolver(_adriadneSolver) {
    	simplifyExprs = true;
    }

    ~TimingSolver() {
      delete solver;
    }

    bool evaluate(const ExecutionState&, ref<Expr>, Solver::Validity &result );

    bool mustBeTrue(const ExecutionState&, ref<Expr>, bool &result);

    bool mustBeFalse(const ExecutionState&, ref<Expr>, bool &result);

    bool mayBeTrue(const ExecutionState&, ref<Expr>, bool &result);

    bool mayBeFalse(const ExecutionState&, ref<Expr>, bool &result);

    bool getValue(const ExecutionState &, ref<Expr> expr, 
                  ref<ConstantExpr> &result);

    bool getInitialValues(const ExecutionState&, 
                          const std::vector<const Array*> &objects,
                          std::vector< std::vector<unsigned char> > &result);

    std::pair< ref<Expr>, ref<Expr> >
        getRange(const ExecutionState&, ref<Expr> query);

    void setTimeout(double t) {
        stpSolver->setTimeout(t);
    }

  };
}

#endif
