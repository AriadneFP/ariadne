//===-- ExprUtil.h ----------------------------------------------*- C++ -*-===//
//
//                     The KLEE Symbolic Virtual Machine
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef KLEE_EXPRUTIL_H
#define KLEE_EXPRUTIL_H

#include <vector>
#include <string>

namespace klee {
  class Array;
  class Expr;
  class ReadExpr;
  template<typename T> class ref;

  /// Find all ReadExprs used in the expression DAG. If visitUpdates
  /// is true then this will including those reachable by traversing
  /// update lists. Note that this may be slow and return a large
  /// number of results.
  void findReads(ref<Expr> e, 
                 bool visitUpdates,
                 std::vector< ref<ReadExpr> > &result);
  
  /// Check if we can find a given variable name.
  bool find(ref<Expr> e, const std::string name);

  /// Symbolic variables are concatentations (using the Concat expression) 
  /// of ReadExpr instances because ReadExpr instances are 8 bytes in length.
  const ReadExpr* extractReadExpr(ref<Expr> e);

  /// Return a list of all unique symbolic objects referenced by the given
  /// expression.
  void findSymbolicObjects(ref<Expr> e,
                           std::vector<const Array*> &results);

  /// Return a list of all unique symbolic objects referenced by the
  /// given expression range.
  template<typename InputIterator>
  void findSymbolicObjects(InputIterator begin, 
                           InputIterator end,
                           std::vector<const Array*> &results);

}

#endif
