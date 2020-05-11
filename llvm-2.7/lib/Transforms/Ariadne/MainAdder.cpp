/*
 * This transformation do the following:
 * - add a main function ( if not exist ) to a module
 * - make the floating point parameters symbolic
 * - call the testing function
 */
#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Instructions.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include <time.h>
#include <sstream>
#include <string>
#include <fstream>

using namespace llvm;

namespace {
  struct MainAdder : public ModulePass {

	Function *klee_make_symbolic;

	std::ofstream inpFile;

	bool fortran;

    static char ID; // Pass identification, replacement for typeid
    MainAdder() : ModulePass(&ID) {}

    bool testPointerElement(const StructType *STy) {
    	bool res = false;
    	for (
    		StructType::element_iterator EB = STy->element_begin(),
    		EI = EB, EE = STy->element_end(); EI != EE; ++EI
    	) {
    		const Type *ty = EI->get();
    		if (ty->isPointerTy())
    			return true;
    		else if (const StructType *sty = dyn_cast<StructType> (ty) ) {
    			res = testPointerElement(sty);
    			if (res)
    				return res;
        	}

    		else if (ty->isArrayTy()) {
        		return true;
        	}
    	}
        return res;
    }

    AllocaInst* allocPointerValue(
    		const PointerType *ptrType, Instruction *nextInst
    ) {
    	srand ( time(NULL) );
    	const Type *orgType = ptrType->getElementType();
    	AllocaInst *alloc = new AllocaInst(orgType, "",nextInst);
    	if (
    		const PointerType *newPtrType = dyn_cast<PointerType> (orgType)
    	) {
    		AllocaInst *newAlloc = allocPointerValue(newPtrType, nextInst);
    		new StoreInst(newAlloc, alloc, nextInst);
    	}
    	//Set the random value for base case here.
    	else {
    		if (orgType == Type::getDoubleTy(nextInst->getContext()) ||
    				orgType == Type::getFloatTy(nextInst->getContext())) {
    			long int M = 10000;
    			double x,r;
    			r = ( (double)rand() / ((double)(RAND_MAX) + (double)(1)));
    			x = (r*M);
    			inpFile << x << std::endl;
    			Constant *constant = ConstantFP::get(orgType,x);
    			new StoreInst(constant, alloc, nextInst);
    		} else if (
    			orgType->isIntegerTy()
    		) {
    			int val = rand();
    			inpFile << val << std::endl;
    			Constant *constant = ConstantInt::get(orgType, val);
    			new StoreInst(constant, alloc, nextInst);
    		}
    	}
    	return alloc;
    }
    virtual bool runOnModule(Module &M) {

      // Get the environment variable to decide change the pointer parameters or not
      if (getenv("__FORTRAN") == NULL)
    	  fortran = false;
      else {
    	  int fortranEnv = atoi(getenv("__FORTRAN"));
    	  if (fortranEnv == 1)
    		  fortran = true;
    	  else
    		  fortran = false;
      }

      klee_make_symbolic = cast<Function>(M.getOrInsertFunction(
    			"klee_make_symbolic",
    			Type::getVoidTy(M.getContext()),
    			PointerType::getUnqual(IntegerType::getInt8Ty(M.getContext())),
    			Type::getInt32Ty(M.getContext()),
    			PointerType::getUnqual(IntegerType::getInt8Ty(M.getContext())),
    			NULL)
      );

      Constant *zero_32 = Constant::getNullValue(
    		  IntegerType::getInt32Ty(M.getContext())
      );

      // Traverse a list of function in a module and print out the output
      for (Module::iterator FF = M.begin(), FE = M.end(); FF != FE; FF++){
    	  Function *function = FF;
    	  // Ignore some functions
    	  if (function->isIntrinsic() || function->isDeclaration() ||
    			  function->getName().compare("fabs") == 0 ||
    			  function->getName().compare("fabsf") == 0 )
    		  continue;
    	  Function* mainFn = cast<Function> (M.getOrInsertFunction(
	      "main", IntegerType::getInt32Ty(M.getContext()), NULL)
    	  );
    	  std::string inpFileName = function->getName().str() + ".rand";
    	  inpFile.open(inpFileName.c_str(), std::fstream::app);
    	  const FunctionType *fnType = function->getFunctionType();
    	  unsigned paramNum = fnType->getNumParams();
    	  std::vector < Value *> params;

    	  // Insert all new instructions before this nextInst
    	  // nextInst should be the terminator of the last basic block
    	  Instruction *nextInst;
    	  // If main is empty function, create a new return instruction
    	  if (mainFn->isDeclaration()){
    		  BasicBlock *entryBB = BasicBlock::Create(
    		      			  M.getContext(), "", mainFn, mainFn->begin()
    		  );
    		  nextInst = ReturnInst::Create(M.getContext(), zero_32, entryBB);
	  } else {
		  Function::iterator BB = mainFn->begin();
		  BasicBlock *lastBB = BB;
		  while ( BB != mainFn->end() ) {
			  lastBB = BB++;
		  }
		  nextInst = lastBB->getTerminator();
	  }

	  Constant *gep_params[] = {
		  zero_32,
		  zero_32
	  };

	  // Write the bitcode file when handling Fortran code that has pointer
	  // parameters in function call
	  bool writeBitCode = true;
	  bool hasFloatingParams = false;
	  for (unsigned i = 0; i < paramNum; i++){
		  const Type* type = fnType->getParamType(i);
		  if (
			  type == Type::getDoubleTy(M.getContext()) ||
			  type == Type::getFloatTy(M.getContext())
		  ) {
			  hasFloatingParams = true;
			  AllocaInst *alloc = new AllocaInst (
    			  		  type, "", nextInst
    			  );
    			  BitCastInst *klee_var = new BitCastInst(
    					  alloc,
    					  Type::getInt8PtrTy(M.getContext()),
    					  "", nextInst
    			  );
    			  std::stringstream s;
    			  s << "__ariadne_tmp" << i;
    			  Constant *msg = ConstantArray::get(
    					  M.getContext(),s.str().c_str() , true
    			  );
    			  GlobalVariable *ariadne_tmp = new GlobalVariable(
    					  M, msg->getType(), true,
    					  GlobalValue::InternalLinkage,
    					  msg , s.str().c_str()
    			  );

    			  Constant *klee_tmp_var = ConstantExpr::getGetElementPtr(
    					  ariadne_tmp, gep_params,
    					  array_lengthof(gep_params)
    			  );

    			  int numOfBytes = 8;
    			  if (type == Type::getFloatPtrTy(M.getContext()))
    				  numOfBytes = 4;
    			  Value *klee_params[] = {
    					  klee_var,
    					  ConstantInt::get(M.getContext(), APInt(32, numOfBytes)),
    					  klee_tmp_var
    			  };
    			  CallInst::Create(
    					  klee_make_symbolic,
    					  klee_params, array_endof(klee_params),
    					  "", nextInst
    			  );
    			  LoadInst *load = new LoadInst (alloc, "", nextInst);
    			  params.push_back(load);
    		  }
    		  else {
    			  if (
    				  const PointerType *ptrType = dyn_cast<PointerType> (type)
    			  ) {
    				  AllocaInst *alloc = allocPointerValue(ptrType, nextInst);
    				  params.push_back(alloc);
    				  // Do not write bitcode if not Fortran and not struct type
    				  if (!ptrType->getElementType()->isStructTy() && !fortran)
    					  writeBitCode = false;
    			  }
    			  //FIXME: at this time, we do not handle pointers in the struct
    			  else if (const StructType *STy = dyn_cast<StructType> (type)) {
    				  if (testPointerElement(STy))
    					  errs() << "At this time, we do not handle the struct type\n";
    				  AllocaInst *alloc = new AllocaInst (
    					  type, "", nextInst
    				  );
    				  LoadInst *load = new LoadInst (alloc, "", nextInst);
    				  params.push_back(load);
    			  }
    			  else if (type->isIntegerTy()) {
    				  srand (time(NULL));
    				  int val = rand();
    				  inpFile << val << std::endl;
    				  errs() << val;
    				  Constant *constant = ConstantInt::get(type, val);
    				  params.push_back(constant);
    			  }
    			  else {
    				  AllocaInst *alloc = new AllocaInst (
    					  type, "", nextInst
    				  );
    				  LoadInst *load = new LoadInst (alloc, "", nextInst);
    				  params.push_back(load);
    			  }
    		  }

    	  }
	  CallInst* function_Call = CallInst::Create(
    			  function,
    			  params.begin(), params.end(),
    			  "", nextInst
	  );

	  function_Call->setCallingConv(function->getCallingConv());
	  /// Only consider the functions containing floating point parameters
	  if (!hasFloatingParams)
		  writeBitCode = false;
	  if (writeBitCode) {
		  std::string outputFileName = "main_" + function->getName().str() + ".o";
		  std::string ErrInfo;
    		  raw_ostream *out = new raw_fd_ostream(
    				  outputFileName.c_str(), ErrInfo, raw_fd_ostream::F_Binary
    		  );
    		  WriteBitcodeToFile(&M,*out);
    	  }
    	  inpFile.close();
	  /// Clean instructions in main
    	  mainFn->eraseFromParent();

      }
      return true;
    }
  };
}

char MainAdder::ID = 0;
static RegisterPass<MainAdder> X("MainAdder", "Main Adder Pass");
