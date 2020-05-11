//===-- ExprPPrinter.cpp -   ----------------------------------------------===//
//
//                     The KLEE Symbolic Virtual Machine
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "klee/util/ExprPPrinter.h"

#include "klee/Constraints.h"
#include "klee/PolyBuilder.h"

#include "llvm/Support/CommandLine.h"

#include <map>
#include <vector>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <float.h>
#include "gmp.h"

using namespace klee;

namespace {
  llvm::cl::opt<bool>
  PCWidthAsArg("pc-width-as-arg", llvm::cl::init(true));

  llvm::cl::opt<bool>
  PCAllWidths("pc-all-widths", llvm::cl::init(false));

  llvm::cl::opt<bool>
  PCPrefixWidth("pc-prefix-width", llvm::cl::init(true));

  llvm::cl::opt<bool>
  PCMultibyteReads("pc-multibyte-reads", llvm::cl::init(true));

  llvm::cl::opt<bool>
  PCAllConstWidths("pc-all-const-widths",  llvm::cl::init(false));
}

/// PrintContext - Helper class for storing extra information for
/// the pretty printer.
class PrintContext {
private:
  std::ostream &os;
  std::stringstream ss;
  std::string newline;

public:
  /// Number of characters on the current line.
  unsigned pos;

public:
  PrintContext(std::ostream &_os) : os(_os), newline("\n"), pos(0) {}

  void setNewline(const std::string &_newline) {
    newline = _newline;
  }

  void breakLine(unsigned indent=0) {
    os << newline;
    if (indent)
      os << std::setw(indent) << ' ';
    pos = indent;
  }

  /// write - Output a string to the stream and update the
  /// position. The stream should not have any newlines.
  void write(const std::string &s) {
    os << s;
    pos += s.length();
  }

  template <typename T>
  PrintContext &operator<<(T elt) {
    ss.str("");
    ss << elt;
    write(ss.str());
    return *this;
  }

  /// Get the ostream
  std::ostream& getOStream() {
	  return os;
  }
};

class PPrinter : public ExprPPrinter {
public:
  std::set<const Array*> usedArrays;
private:
  std::map<ref<Expr>, unsigned> bindings;
  std::map<const UpdateNode*, unsigned> updateBindings;
  std::set< ref<Expr> > couldPrint, shouldPrint;
  std::set<const UpdateNode*> couldPrintUpdates, shouldPrintUpdates;
  std::ostream &os;
  unsigned counter;
  unsigned updateCounter;
  bool hasScan;
  std::string newline;

  /// shouldPrintWidth - Predicate for whether this expression should
  /// be printed with its width.
  bool shouldPrintWidth(ref<Expr> e) {
    if (PCAllWidths)
      return true;
    return e->getWidth() != Expr::Bool;
  }

  bool isVerySimple(const ref<Expr> &e) { 
    return isa<ConstantExpr>(e) || bindings.find(e)!=bindings.end();
  }

  bool isVerySimpleUpdate(const UpdateNode *un) {
    return !un || updateBindings.find(un)!=updateBindings.end();
  }


  // document me!
  bool isSimple(const ref<Expr> &e) { 
    if (isVerySimple(e)) {
      return true;
    } else if (const ReadExpr *re = dyn_cast<ReadExpr>(e)) {
      return isVerySimple(re->index) && isVerySimpleUpdate(re->updates.head);
    } else {
      Expr *ep = e.get();
      for (unsigned i=0; i<ep->getNumKids(); i++)
        if (!isVerySimple(ep->getKid(i)))
          return false;
      return true;
    }
  }

  bool hasSimpleKids(const Expr *ep) {
      for (unsigned i=0; i<ep->getNumKids(); i++)
        if (!isSimple(ep->getKid(i)))
          return false;
      return true;
  }
  
  void scanUpdate(const UpdateNode *un) {
    // FIXME: This needs to be non-recursive.
    if (un) {
      if (couldPrintUpdates.insert(un).second) {
        scanUpdate(un->next);
        scan1(un->index);
        scan1(un->value);
      } else {
        shouldPrintUpdates.insert(un);
      }
    }
  }

  void scan1(const ref<Expr> &e) {
    if (!isa<ConstantExpr>(e)) {
      if (couldPrint.insert(e).second) {
        Expr *ep = e.get();
        for (unsigned i=0; i<ep->getNumKids(); i++)
          scan1(ep->getKid(i));
        if (const ReadExpr *re = dyn_cast<ReadExpr>(e)) {
          usedArrays.insert(re->updates.root);
          scanUpdate(re->updates.head);
        }
      } else {
        shouldPrint.insert(e);
      }
    }
  }

  void printUpdateList(const UpdateList &updates, PrintContext &PC) {
    const UpdateNode *head = updates.head;

    // Special case empty list.
    if (!head) {
      // FIXME: We need to do something (assert, mangle, etc.) so that printing
      // distinct arrays with the same name doesn't fail.
      PC << updates.root->name;
      return;
    }

    // FIXME: Explain this breaking policy.
    bool openedList = false, nextShouldBreak = false;
    unsigned outerIndent = PC.pos;
    unsigned middleIndent = 0;
    for (const UpdateNode *un = head; un; un = un->next) {      
      // We are done if we hit the cache.
      std::map<const UpdateNode*, unsigned>::iterator it = 
        updateBindings.find(un);
      if (it!=updateBindings.end()) {
        if (openedList)
          PC << "] @ ";
        PC << "U" << it->second;
        return;
      } else if (!hasScan || shouldPrintUpdates.count(un)) {
        if (openedList)
          PC << "] @";
        if (un != head)
          PC.breakLine(outerIndent);
        PC << "U" << updateCounter << ":"; 
        updateBindings.insert(std::make_pair(un, updateCounter++));
        openedList = nextShouldBreak = false;
     }
    
      if (!openedList) {
        openedList = 1;
        PC << '[';
        middleIndent = PC.pos;
      } else {
        PC << ',';
        printSeparator(PC, !nextShouldBreak, middleIndent);
      }
      //PC << "(=";
      //unsigned innerIndent = PC.pos;
      print(un->index, PC);
      //printSeparator(PC, isSimple(un->index), innerIndent);
      PC << "=";
      print(un->value, PC);
      //PC << ')';
      
      nextShouldBreak = !(isa<ConstantExpr>(un->index) && 
                          isa<ConstantExpr>(un->value));
    }

    if (openedList)
      PC << ']';

    PC << " @ " << updates.root->name;
  }

  void printWidth(PrintContext &PC, ref<Expr> e) {
    if (!shouldPrintWidth(e))
      return;

    if (PCWidthAsArg) {
      PC << ' ';
      if (PCPrefixWidth)
        PC << 'w';
    }

    PC << e->getWidth();
  }

  
  bool isReadExprAtOffset(ref<Expr> e, const ReadExpr *base, ref<Expr> offset) {
    const ReadExpr *re = dyn_cast<ReadExpr>(e.get());
      
    // right now, all Reads are byte reads but some
    // transformations might change this
    if (!re || (re->getWidth() != Expr::Int8))
      return false;
      
    // Check if the index follows the stride. 
    // FIXME: How aggressive should this be simplified. The
    // canonicalizing builder is probably the right choice, but this
    // is yet another area where we would really prefer it to be
    // global or else use static methods.
    return SubExpr::create(re->index, base->index) == offset;
  }
  
  
  /// hasOrderedReads: \arg e must be a ConcatExpr, \arg stride must
  /// be 1 or -1.  
  ///
  /// If all children of this Concat are reads or concats of reads
  /// with consecutive offsets according to the given \arg stride, it
  /// returns the base ReadExpr according to \arg stride: first Read
  /// for 1 (MSB), last Read for -1 (LSB).  Otherwise, it returns
  /// null.
  const ReadExpr* hasOrderedReads(ref<Expr> e, int stride) {
    assert(e->getKind() == Expr::Concat);
    assert(stride == 1 || stride == -1);
    
    const ReadExpr *base = dyn_cast<ReadExpr>(e->getKid(0));
    
    // right now, all Reads are byte reads but some
    // transformations might change this
    if (!base || base->getWidth() != Expr::Int8)
      return NULL;
    
    // Get stride expr in proper index width.
    Expr::Width idxWidth = base->index->getWidth();
    ref<Expr> strideExpr = ConstantExpr::alloc(stride, idxWidth);
    ref<Expr> offset = ConstantExpr::create(0, idxWidth);
    
    e = e->getKid(1);
    
    // concat chains are unbalanced to the right
    while (e->getKind() == Expr::Concat) {
      offset = AddExpr::create(offset, strideExpr);
      if (!isReadExprAtOffset(e->getKid(0), base, offset))
	return NULL;
      
      e = e->getKid(1);
    }
    
    offset = AddExpr::create(offset, strideExpr);
    if (!isReadExprAtOffset(e, base, offset))
      return NULL;
    
    if (stride == -1)
      return cast<ReadExpr>(e.get());
    else return base;
  }

#if 0
  /// hasAllByteReads - True iff all children are byte level reads or
  /// concats of byte level reads.
  bool hasAllByteReads(const Expr *ep) {
    switch (ep->kind) {
      Expr::Read: {
	// right now, all Reads are byte reads but some
	// transformations might change this
	return ep->getWidth() == Expr::Int8;
      }
      Expr::Concat: {
	for (unsigned i=0; i<ep->getNumKids(); ++i) {
	  if (!hashAllByteReads(ep->getKid(i)))
	    return false;
	}
      }
    default: return false;
    }
  }
#endif

  void printRead(const ReadExpr *re, PrintContext &PC, unsigned indent) {
    print(re->index, PC);
    printSeparator(PC, isVerySimple(re->index), indent);
    printUpdateList(re->updates, PC);
  }
  
  void printRead_SMT(const ReadExpr *re, PrintContext &PC, unsigned indent){
  	//print(re->index, PC);
  	printSeparator(PC, isVerySimple(re->index), indent);
  	printUpdateList(re->updates, PC);
  }

  void printExtract(const ExtractExpr *ee, PrintContext &PC, unsigned indent) {
    PC << ee->offset << ' ';
    print(ee->expr, PC);
  }

  void printExpr(
	const Expr *ep, PrintContext &PC, unsigned indent, bool printConstWidth=false
  ) {

    bool simple = hasSimpleKids(ep);
    
    print(ep->getKid(0), PC);
    for (unsigned i=1; i<ep->getNumKids(); i++) {
      printSeparator(PC, simple, indent);
      print(ep->getKid(i), PC, printConstWidth);
    }
  }
  
  void printExprSMT(
	const Expr *ep, PrintContext &PC, unsigned indent
  ) {

    bool simple = hasSimpleKids(ep);
    printSMT_LIB(ep->getKid(0), PC);
	for (unsigned i=1; i<ep->getNumKids(); i++) {
      printSeparator(PC, simple, indent);
      printSMT_LIB(ep->getKid(i), PC);
    }
  }

public:
  PPrinter(std::ostream &_os) : os(_os), newline("\n") {
    reset();
  }

  void setNewline(const std::string &_newline) {
    newline = _newline;
  }

  void reset() {
    counter = 0;
    updateCounter = 0;
    hasScan = false;
    bindings.clear();
    updateBindings.clear();
    couldPrint.clear();
    shouldPrint.clear();
    couldPrintUpdates.clear();
    shouldPrintUpdates.clear();
  }

  void scan(const ref<Expr> &e) {
    hasScan = true;
    scan1(e);
  }

  void print(const ref<Expr> &e, unsigned level=0) {
    PrintContext PC(os);
    PC.pos = level;
    print(e, PC);
  }

  void printSMT_LIB(const ref<Expr> &e, unsigned level=0) {
    PrintContext PC(os);
    PC.pos = level;
    printSMT_LIB(e, PC);
  }

  void printConst(const ref<ConstantExpr> &e, PrintContext &PC, 
                  bool printWidth) {
    if (e->getWidth() == Expr::Bool)
      PC << (e->isTrue() ? "true" : "false");
    else {
      if (PCAllConstWidths)
    	  printWidth = true;
    
      if (printWidth)
    	  PC << "(w" << e->getWidth() << " ";

      std::string S;
      e->toString(S);
      PC << S;

      if (printWidth)
    	  PC << ")";
    }    
  }

  void printConstSMT(const ref<ConstantExpr> &e, PrintContext &PC) {
      if (e->getWidth() == Expr::Bool)
        PC << (e->isTrue() ? "true" : "false");
      else {
        std::string S;
        e->toString(S);
        /// Handle infinity values
        if (S.find("-Inf") != std::string::npos) {
        	mpq_t nmax;
        	mpq_init(nmax);
        	mpq_set_d(nmax, -DBL_MAX);
        	PC << mpq_get_str(NULL, 10, nmax);
        } else if (S.find("Inf") != std::string::npos) {
        	mpq_t max;
			mpq_init(max);
			mpq_set_d(max, DBL_MAX);
			PC << mpq_get_str(NULL, 10, max);
        } else
        	PC << S;
      }
  }

  //Extract ReadExpr from a Concat expression
  const ReadExpr* extractReadExpr(ref<Expr> e) {
      assert(e->getKind() == Expr::Concat);

      const ReadExpr *base = dyn_cast<ReadExpr>(e->getKid(0));

      // right now, all Reads are byte reads but some
      // transformations might change this
      if (!base)
        return false;
      return base;
  }

  void print(const ref<Expr> &e, PrintContext &PC, bool printConstWidth=false) {
    if (ConstantExpr *CE = dyn_cast<ConstantExpr>(e))
      printConst(CE, PC, printConstWidth);
    else {
      std::map<ref<Expr>, unsigned>::iterator it = bindings.find(e);
      if (it!=bindings.end()) {
        PC << 'N' << it->second;
      } else {
        if (!hasScan || shouldPrint.count(e)) {
          PC << 'N' << counter << ':';
          bindings.insert(std::make_pair(e, counter++));
        }

        // Detect multibyte reads.
        // FIXME: Hrm. One problem with doing this is that we are
        // masking the sharing of the indices which aren't
        // visible. Need to think if this matters... probably not
        // because if they are offset reads then its either constant,
        // or they are (base + offset) and base will get printed with
        // a declaration.
        //if (PCMultibyteReads && e->getKind() == Expr::Concat) {
        if (e->getKind() == Expr::Concat) {
        	const ReadExpr *base = extractReadExpr(e);
        	bool isLSB = true;
        	PC << "(Read" << (isLSB ? "LSB" : "MSB");
        	printWidth(PC, e);
        	//PC << ' ';
        	printRead_SMT(base, PC, PC.pos);
        	PC << ')';
        	return;
        }

        PC << '(' << e->getKind();
        printWidth(PC, e);
        PC << ' ';

        // Indent at first argument and dispatch to appropriate print
        // routine for exprs which require special handling.
        unsigned indent = PC.pos;
        if (const ReadExpr *re = dyn_cast<ReadExpr>(e)) {
          printRead(re, PC, indent);
        } else if (const ExtractExpr *ee = dyn_cast<ExtractExpr>(e)) {
          printExtract(ee, PC, indent);
        } else if (e->getKind() == Expr::Concat || e->getKind() == Expr::SExt)
	  printExpr(e.get(), PC, indent, true);
	else
          printExpr(e.get(), PC, indent);	
        PC << ")";
      }
    }
  }

  void printConst_iSAT(const ref<ConstantExpr> &e, PrintContext &PC) {
	  if (e->getWidth() == Expr::Bool)
	    PC << (e->isTrue() ? "true" : "false");
	  else {
        std::string S;
  		e->toString(S);
  		PC << S;
      }
  }

  void print_iSAT(
	  const ref<Expr> &e, PrintContext &PC, bool printConstWidth = false
  ) {
  	  if (ConstantExpr *CE = dyn_cast<ConstantExpr>(e)) {
			printConst_iSAT(CE, PC);
			return;
  	  }
  	  else {
			if (e->getKind() == Expr::Concat) {
				const ReadExpr *base = extractReadExpr(e);
				if (base) {
					printRead_SMT(base, PC, PC.pos);
					return;
				}
			}
  	  }
  	  unsigned indent = PC.pos;
  	  switch (e-> getKind()) {
		// Comparison operations
		case Expr::Slt:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " < ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;
		case Expr::Ult:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " < ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;
		case Expr::Ule:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " <= ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;
		case Expr::Sle:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " <= ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;
		case Expr::Eq:
			PC << "( ";
			if (e->getKid(0)->getWidth() == Expr::Bool
					&& e->getKid(0)->isTrue()) {
				if (e->getKid(0)->getWidth() == Expr::Bool &&
						e->getKid(1)->isTrue())
					PC << " true ";
				else if (e->getKid(1)->getWidth() == Expr::Bool &&
						e->getKid(1)->isFalse())
					PC << " false ";
				else {
					print_iSAT(e->getKid(1), PC, indent);
				}
			} else if (e->getKid(0)->getWidth() == Expr::Bool &&
					e->getKid(0)->isFalse()) {
				if (e->getKid(1)->getWidth() == Expr::Bool &&
						e->getKid(1)->isTrue())
					PC << " false ";
				else if (e->getKid(1)->getWidth() == Expr::Bool &&
						e->getKid(1)->isFalse())
					PC << " true ";
				else {
					PC << " ! ";
					print_iSAT(e->getKid(1), PC, indent);
				}
			} else if (e->getKid(1)->getWidth() == Expr::Bool &&
					e->getKid(1)->isTrue()) {
				print_iSAT(e->getKid(0), PC, indent);
			} else if (e->getKid(1)->getWidth() == Expr::Bool &&
					e->getKid(1)->isFalse()) {
				PC << " ! ";
				print_iSAT(e->getKid(0), PC, indent);
			} else {
				print_iSAT(e->getKid(0), PC, indent);
				PC << " = ";
				print_iSAT(e->getKid(1), PC, indent);
			}
			PC << ")";
			break;
		case Expr::Ne:
			PC << "( ";
			if (e->getKid(0)->getWidth() == Expr::Bool &&
					e->getKid(0)->isTrue()) {
				if (e->getKid(1)->isTrue())
					PC << " false ";
				else if ( e->getKid(1)->getWidth() == Expr::Bool &&
						e->getKid(1)->isFalse())
					PC << " true ";
				else {
					PC << " ! ";
					print_iSAT(e->getKid(1), PC, indent);
				}
			} else if (e->getKid(0)->getWidth() == Expr::Bool &&
					e->getKid(0)->isFalse() ) {
				if (e->getKid(1)->getWidth() == Expr::Bool &&
						e->getKid(1)->isTrue())
					PC << " true ";
				else if ( e->getKid(1)->getWidth() == Expr::Bool &&
						e->getKid(1)->isFalse())
					PC << " false ";
				else {
					print_iSAT(e->getKid(1), PC, indent);
				}
			} else if (e->getKid(1)->getWidth() == Expr::Bool &&
					e->getKid(1)->isTrue()) {
				PC << " ! ";
				print_iSAT(e->getKid(0), PC, indent);
			} else if  (e->getKid(1)->getWidth() == Expr::Bool &&
					e->getKid(1)->isFalse()) {
				print_iSAT(e->getKid(0), PC, indent);
			}
			else {
				print_iSAT(e->getKid(0), PC, indent);
				PC << " != ";
				print_iSAT(e->getKid(1), PC, indent);
			}
			PC << ")";
			break;
		case Expr::Sgt:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " > ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;
		case Expr::Ugt:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " > ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;
		case Expr::Uge:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " >= ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;
		case Expr::Sge:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " >= ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;

		/// Arithmetic operations
		case Expr::Add:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " + ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;
		case Expr::Sub:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " - ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;
		case Expr::Mul:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " * ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;
		case Expr::UDiv:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " / ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;
		case Expr::SDiv:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " / ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;
		case Expr::Div:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " / ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;

		/// Logical expressions
		case Expr::Not:
			PC << "(! ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << ")";
			break;

		case Expr::And:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " and ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;

		case Expr::Or:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " or ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;

		case Expr::Xor:
			PC << "( ";
			print_iSAT(e->getKid(0), PC, indent);
			PC << " xor ";
			print_iSAT(e->getKid(1), PC, indent);
			PC << ")";
			break;

		/*case Expr::ZExt:
			PC << e << ")";
			PC.breakLine();
			break;
		case Expr::SExt:
			PC << e << ")";
			PC.breakLine();
			break;*/
		default:
			break;
		}
  }

  void print_Z3( const ref<Expr> &e, PrintContext &PC ) {

  	if (ConstantExpr *CE = dyn_cast<ConstantExpr>(e))
  		printConstSMT(CE, PC);
    else {
       	if (e->getKind() == Expr::Concat) {
       		const ReadExpr *base = extractReadExpr(e);
       		if (base) {
       			printRead_SMT(base, PC, PC.pos);
       			return;
       		}
       	}
  	}
  	unsigned indent = PC.pos;
  	//FIXME: handle all kinds of expressions
  	//int selectIndex = 0;
  	switch (e-> getKind()) {
  		/// Comparison operations
  		case Expr::Slt:
  			PC << "(< ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;
  		case Expr::Ult:
  			PC << "(< ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;
  		case Expr::Ule:
  			PC << "(<= ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;
  		case Expr::Eq:
  			PC << "(= ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;
  		case Expr::Sgt:
  			PC << "(> ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;
  		case Expr::Ugt:
  			PC << "(> ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;
  		case Expr::Uge:
  			PC << "(> ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;
  		case Expr::Sle:
  			PC << "(<= ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;
  		case Expr::Sge:
  			PC << "(>= ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;

  		/// Arithmetic operations
  		case Expr::Add:
  			PC << "(+ ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;
  		case Expr::Sub:
  			PC << "(- ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;
  		case Expr::Mul:
  			PC << "(* ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;
  		case Expr::UDiv:
  			PC << "(/ ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;
  		case Expr::SDiv:
  			PC << "(/ ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;
  		case Expr::Div:
  			PC << "(/ ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;

  		/// Logical expressions
  		case Expr::Not:
  			PC << "(not ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;

  		case Expr::And:
  			PC << "(and ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;

  		case Expr::Or:
  			PC << "(or ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;

  		case Expr::Xor:
  			PC << "(xor ";
  			printExprSMT(e.get(), PC, indent);
  			PC << ")";
  			PC.breakLine(indent);
  			break;

  		/*case Expr::ZExt:
  			PC << e << ")";
  			PC.breakLine();
  			break;
  		case Expr::SExt:
  			PC << e << ")";
  			PC.breakLine();
  			break;

  		/// FIXME: select expression
  		case Expr::Select:
  			if (SelectExpr *selectExpr = dyn_cast< SelectExpr >(e))
  				selectIndex = selectExpr->getSymbolicIndex();
  			PC << "select" << selectIndex;
  			break;*/

  		default:
  			/*PC << e << ")";
  			PC.breakLine();*/
  			break;
		}
  }

  void printSMT_LIB(const ref<Expr> &e, PrintContext &PC) {
    
	if (ConstantExpr *CE = dyn_cast<ConstantExpr>(e)) {
		printConstSMT(CE, PC);
		return;
	}
	else {
      	if (e->getKind() == Expr::Concat) {
	  		const ReadExpr *base = extractReadExpr(e);
	  		if (base) {
	    		printRead_SMT(base, PC, PC.pos);
	    		return;
	  		}
      	}
	}

	unsigned indent = PC.pos;

	//FIXME: handle all kinds of expressions
	int selectIndex = 0;
	switch (e-> getKind()){
		//Compare		
		case Expr::Slt:
			PC << "(< ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;
		case Expr::Ult:
			PC << "(< ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;
		case Expr::Ule:
			PC << "(<= ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;
		case Expr::Eq:
			PC << "(= ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;
		case Expr::Sgt:
			PC << "(> ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;
		case Expr::Ugt:
			PC << "(> ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;
		case Expr::Uge:
			PC << "(> ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;
		case Expr::Sle:
			PC << "(<= ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;
		case Expr::Sge:
			PC << "(>= ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;

		/// Arithmetic operations
		case Expr::Add:
			PC << "(+ ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;
		case Expr::Sub:
			PC << "(- ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;
		case Expr::Mul:
			PC << "(* ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;
		case Expr::UDiv:
			PC << "(/ ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;
		case Expr::SDiv:
			PC << "(/ ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;
		case Expr::Div:
			PC << "(/ ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;

		/// Logical expressions
		case Expr::Not:
			PC << "(not ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;

		case Expr::And:
			PC << "(and ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;

		case Expr::Or:
			PC << "(or ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;

		case Expr::Xor:
			PC << "(xor ";
			printExprSMT(e.get(), PC, indent);
			PC << ")";
			break;

		case Expr::ZExt:
			PC << e << ")";
			PC.breakLine();
			break;
		case Expr::SExt:
			PC << e << ")";
			PC.breakLine();
			break;

		/// FIXME: select expression
		case Expr::Select:
			if (SelectExpr *selectExpr = dyn_cast< SelectExpr >(e))
				selectIndex = selectExpr->getSymbolicIndex();
			PC << "select" << selectIndex;
			break;

		default:
			PC << e << "\n";
			PC.breakLine();
			break; 	
	}    
  }


  /* Public utility functions */

  void printSeparator(PrintContext &PC, bool simple, unsigned indent) {
    if (simple) {
      PC << ' ';
    } else {
      PC.breakLine(indent);
    }
  }
};

ExprPPrinter *klee::ExprPPrinter::create(std::ostream &os) {
  return new PPrinter(os);
}

void ExprPPrinter::printOne(std::ostream &os,
                            const char *message, 
                            const ref<Expr> &e) {
  PPrinter p(os);
  p.scan(e);

  // FIXME: Need to figure out what to do here. Probably print as a
  // "forward declaration" with whatever syntax we pick for that.
  PrintContext PC(os);
  PC << message << ": ";
  p.print(e, PC);
  PC.breakLine();
}

void ExprPPrinter::printSingleExpr(std::ostream &os, const ref<Expr> &e) {
  PPrinter p(os);
  p.scan(e);

  // FIXME: Need to figure out what to do here. Probably print as a
  // "forward declaration" with whatever syntax we pick for that.
  PrintContext PC(os);
  p.print(e, PC);
}

void ExprPPrinter::printConstraints(std::ostream &os,
                                    const ConstraintManager &constraints) {
  printQuery(os, constraints, ConstantExpr::alloc(false, Expr::Bool));
}


void ExprPPrinter::printQuery(std::ostream &os,
                              const ConstraintManager &constraints,
                              const ref<Expr> &q,
                              const ref<Expr> *evalExprsBegin,
                              const ref<Expr> *evalExprsEnd,
                              const Array * const *evalArraysBegin,
                              const Array * const *evalArraysEnd,
                              bool printArrayDecls) {
  PPrinter p(os);
  
  for (ConstraintManager::const_iterator it = constraints.begin(),
         ie = constraints.end(); it != ie; ++it)
    p.scan(*it);
  p.scan(q);

  for (const ref<Expr> *it = evalExprsBegin; it != evalExprsEnd; ++it)
    p.scan(*it);

  PrintContext PC(os);
  
  // Print array declarations.
  if (printArrayDecls) {
    for (const Array * const* it = evalArraysBegin; it != evalArraysEnd; ++it)
      p.usedArrays.insert(*it);
    for (std::set<const Array*>::iterator it = p.usedArrays.begin(), 
           ie = p.usedArrays.end(); it != ie; ++it) {
      const Array *A = *it;
      // FIXME: Print correct name, domain, and range.
      PC << "array " << A->name
         << "[" << A->size << "]"
         << " : " << "w32" << " -> " << "w8" << " = ";
      if (A->isSymbolicArray()) {
        PC << "symbolic";
      } else {
        PC << "[";
        for (unsigned i = 0, e = A->size; i != e; ++i) {
          if (i)
            PC << " ";
          PC << A->constantValues[i];
        }
        PC << "]";
      }
      PC.breakLine();
    }
  }

  PC << "(query [";
  
  // Ident at constraint list;
  unsigned indent = PC.pos;
  for (ConstraintManager::const_iterator it = constraints.begin(),
         ie = constraints.end(); it != ie;) {
    p.print(*it, PC);
    ++it;
    if (it != ie)
      PC.breakLine(indent);
  }
  PC << ']';

  p.printSeparator(PC, constraints.empty(), indent-1);
  p.print(q, PC);

  // Print expressions to obtain values for, if any.
  if (evalExprsBegin != evalExprsEnd) {
    p.printSeparator(PC, q->isFalse(), indent-1);
    PC << '[';
    for (const ref<Expr> *it = evalExprsBegin; it != evalExprsEnd; ++it) {
      p.print(*it, PC, /*printConstWidth*/true);
      if (it + 1 != evalExprsEnd)
        PC.breakLine(indent);
    }
    PC << ']';
  }

  // Print arrays to obtain values for, if any.
  if (evalArraysBegin != evalArraysEnd) {
    if (evalExprsBegin == evalExprsEnd)
      PC << " []";

    PC.breakLine(indent - 1);
    PC << '[';
    for (const Array * const* it = evalArraysBegin; it != evalArraysEnd; ++it) {
      PC << (*it)->name;
      if (it + 1 != evalArraysEnd)
        PC.breakLine(indent);
    }
    PC << ']';
  }

  PC << ')';
  PC.breakLine();
}

void collectSelectExprs(
	ref<Expr> e, std::set<SelectExpr> &selectExprList,
	std::set<int> &selectIndexList
) {
	if (SelectExpr *selectExpr = dyn_cast< SelectExpr >(e)) {
		if (selectIndexList.find(selectExpr->getSymbolicIndex()) == selectIndexList.end()) {
			selectIndexList.insert(selectExpr->getSymbolicIndex());
			selectExprList.insert(*selectExpr);
		}
		return;
	} else {
		Expr *ep = e.get();
		for (unsigned i = 0; i < ep->getNumKids(); i++) {
			collectSelectExprs((ep->getKid(i)), selectExprList, selectIndexList);
		}
	}
}

/// Print the select expression in SMTLIB format
void printSelectSMT(
	const SelectExpr selectExpr, PrintContext &PC, unsigned indent
) {
	PPrinter p(PC.getOStream());
	PC << "(or ( and (= true ";
	p.printSMT_LIB(selectExpr.getKid(0), PC);
	PC << ")";
	PC.breakLine(indent);
	PC << "(= select" << selectExpr.getSymbolicIndex() << " ";
	p.printSMT_LIB(selectExpr.getKid(1), PC);
	PC << "))";
	PC.breakLine(indent);
	PC << "(and (= false ";
	p.printSMT_LIB(selectExpr.getKid(0), PC);
	PC << ")";
	PC.breakLine(indent);
	PC << "(= select" << selectExpr.getSymbolicIndex() << " ";
	p.printSMT_LIB(selectExpr.getKid(2), PC);
	PC << ")))";
	PC.breakLine(indent);
}

//The method to print to a standard SMT-LIB file
void ExprPPrinter::printQuery_SMT(std::ostream &os,
                              const ConstraintManager &constraints,
                              const ref<Expr> &q,
                              const ref<Expr> *evalExprsBegin,
                              const ref<Expr> *evalExprsEnd,
                              const Array * const *evalArraysBegin,
                              const Array * const *evalArraysEnd,
                              bool printArrayDecls) {
  PPrinter p(os);
  
  for (ConstraintManager::const_iterator it = constraints.begin(),
         ie = constraints.end(); it != ie; ++it)
    p.scan(*it);
  p.scan(q);

  for (const ref<Expr> *it = evalExprsBegin; it != evalExprsEnd; ++it)
    p.scan(*it);

  PrintContext PC(os);
  
  std::set<SelectExpr> selectExprList;
  std::set<int> selectIndexList;

  /// Collect and print the list of all select instructions
  for (ConstraintManager::const_iterator it = constraints.begin(),
           ie = constraints.end(); it != ie; ++it)
	  collectSelectExprs(*it, selectExprList, selectIndexList);

  //Print theory declarations
  os << "(benchmark arithmetic" << std::endl;
  os << ":source {unknown}" << std::endl;
  os << ":logic QF_UFLRA" << std::endl;
  
  /// Print select variables
  for (
	std::set<SelectExpr>::iterator it = selectExprList.begin();
	it != selectExprList.end(); it++
  ) {
	  SelectExpr selectExpr = *it;
	  PC << ":extrafuns ((select" << selectExpr.getSymbolicIndex() << " Real ))";
	  PC.breakLine();
  }

  //Print variable declarations
  if (printArrayDecls) {
    for (const Array * const* it = evalArraysBegin; it != evalArraysEnd; ++it)
      p.usedArrays.insert(*it);
    for (std::set<const Array*>::iterator it = p.usedArrays.begin(), 
           ie = p.usedArrays.end(); it != ie; ++it) {
      const Array *A = *it;
      PC << ":extrafuns ((" << A->name << " Real ))";
      PC.breakLine();
    }
  }

  // Print formulas
  PC << ":formula" ;
  PC.breakLine();
  PC << "(and " ;

  // Ident at constraint list;
  unsigned indent = PC.pos;

  for (
	std::set<SelectExpr>::iterator it = selectExprList.begin();
	it != selectExprList.end(); it++
  ) {
	  SelectExpr selectExpr = *it;
	  printSelectSMT(selectExpr, PC, indent);
  }

  for (ConstraintManager::const_iterator it = constraints.begin(),
         ie = constraints.end(); it != ie;) {
    p.printSMT_LIB(*it, PC);
	++it;
    if (it != ie)
      PC.breakLine(indent);
  }

  // Print expressions to obtain values for, if any.
  if (evalExprsBegin != evalExprsEnd) {
    p.printSeparator(PC, q->isFalse(), indent-1);
    for (const ref<Expr> *it = evalExprsBegin; it != evalExprsEnd; ++it) {
      p.printSMT_LIB(*it, PC);
      if (it + 1 != evalExprsEnd)
        PC.breakLine(indent);
    }
  }
  
  //end of SMT file
  PC << ")";
  PC.breakLine();
  PC << ")";
  
}
void ExprPPrinter::printSMT_LIB(std::ostream &os,
								const ConstraintManager &constraints)
{
	printQuery_SMT(os, constraints, ConstantExpr::alloc(false, Expr::Bool));
}

void ExprPPrinter::print_iSAT( std::ostream &os,
								const ConstraintManager &constraints){
	PPrinter p(os);

	for (ConstraintManager::const_iterator it = constraints.begin(),
	         ie = constraints.end(); it != ie; ++it)
	    p.scan(*it);
	PrintContext PC(os);
	//Print declarations
	os << "DECL" << std::endl;
	for (std::set<const Array*>::iterator it = p.usedArrays.begin(),
	           ie = p.usedArrays.end(); it != ie; ++it) {
	     const Array *A = *it;
	     os << "float [-2147483646,2147483646] " << A->name << ";" << std::endl;
	}
	//Print Expressions
	os << "EXPR" << std::endl;
	for (ConstraintManager::const_iterator it = constraints.begin(),
	         ie = constraints.end(); it != ie; it++) {
	    p.print_iSAT(*it, PC);
	    os << ";" << std::endl;
	}
}

void ExprPPrinter::print_Z3( std::ostream &os,
								const ConstraintManager &constraints) {
	PPrinter p(os);

	for (ConstraintManager::const_iterator it = constraints.begin(), ie =
			constraints.end(); it != ie; ++it)
		p.scan(*it);
	PrintContext PC(os);
	/// Print declarations
	os << "(set-logic QF_UFLRA)" << std::endl;
	os << "(declare-funs" << std::endl;
	os << "(" << std::endl;
	for (std::set<const Array*>::iterator it = p.usedArrays.begin(), ie =
			p.usedArrays.end(); it != ie; ++it) {
		const Array *A = *it;
		os << "(" << A->name << " Real)" << std::endl;
	}
	os << ")" << std::endl;
	os << ")" << std::endl;

	/// Print the constraints
	for (ConstraintManager::const_iterator it = constraints.begin(), ie =
			constraints.end(); it != ie; it++) {
		os << "(assert" << std::endl;
		p.print_Z3(*it, PC);
		os << ")" << std::endl;
	}

	/// Print assertion check
	os << "(check-sat)" << std::endl;
	os << "(exit)" << std::endl;

}

void ExprPPrinter::printSingleExpr_Poly(std::ostream &os, const ref<Expr> &exp) {
	Poly nom,denom,p;
	Poly::CompareType type;
	exp->toPolyFraction(nom,denom,type);
	p = nom.multiply(denom);
	p.print(os);
	switch (type){
		case Poly::Eq:
			os << " = 0";
			break;
		case Poly::Ge:
			os << " >= 0";
			break;
		case Poly::Gt:
			os << " > 0";
			break;
		case Poly::Le:
			os << " <= 0";
			break;
		case Poly::Lt:
			os << " < 0";
			break;
		case Poly::Ne:
			os << " != 0";
			break;
		default:
			break;
	}
	os << std::endl;
}

void ExprPPrinter::print_Poly( std::ostream &os,
								const ConstraintManager &constraints){
	for (ConstraintManager::const_iterator it = constraints.begin(),
				ie = constraints.end(); it != ie; ++it){
		ref<Expr> exp = *it;
		printSingleExpr_Poly(os, exp);
	}
}
