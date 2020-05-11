//===----------------------------------------------------------------------===//
//
// This file implements the transformation methods for making floating point exceptions
// explicit
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "print"
#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Function.h"
#include "llvm/Assembly/Writer.h"
#include "llvm/Assembly/AsmAnnotationWriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Bitcode/ReaderWriter.h"

using namespace llvm;


namespace {
  struct Print: public FunctionPass{
	  static char ID; // Pass identification, replacement for typeid

	  Print() : FunctionPass(&ID) { }

      AssemblyAnnotationWriter w;

	  virtual bool runOnFunction(Function &F){

		  //F.print(errs(),&w);
		  return true;
	  }

	  virtual bool doInitialization(Module &M){

		  return true;
	  }

	  virtual bool doFinalization(Module &M){
		  M.print(errs(),&w);
		  return true;
	  }

  };
}
char Print::ID = 0;
static RegisterPass<Print> X("Print", "Print bitcode to test the transformation Pass");
