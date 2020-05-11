/*
 * LogLine.cpp: Add main to log line transformed code
 *
 *  Created on: Feb 22, 2011
 *      Author: thanh
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
#include "llvm/Assembly/AsmAnnotationWriter.h"
#include <sstream>
#include <string>

using namespace llvm;

namespace {
  struct LogLineMain : public ModulePass {

	Function *printf;
	Function *mainFn;
	Function *mpq_set_str;
	Function *mpq_init;
	Function *exit;
	Function *feenableexcept;
	const StructType *mpq;
	const Type *mpq_Ptr;
	Function *mpq_get_d;
	GlobalVariable *floatMsg;
	GlobalVariable *intMsg;
	GlobalVariable *inputMsg;
	GlobalVariable *lfMsg;
	AssemblyAnnotationWriter w;
	Constant *zero_32;
	const Type *int32;

    static char ID; // Pass identification, replacement for typeid
    LogLineMain() : ModulePass(&ID) {}

    void init(Module &M) {

    	//Declare printf function
    	std::vector<const Type*> ft_printf_args;
    	ft_printf_args.push_back(
    			PointerType::getUnqual(IntegerType::getInt8Ty(M.getContext()))
    	);

    	FunctionType* ft_printf = FunctionType::get(
    			IntegerType::getInt32Ty(M.getContext()), ft_printf_args, true
    	);

    	printf = cast<Function>(
    			M.getOrInsertFunction("printf", ft_printf)
    	);

    	zero_32 = Constant::getNullValue(
    			IntegerType::getInt32Ty(M.getContext())
    	);

    	int32 = Type::getInt32Ty(M.getContext());

    	Constant *input_msg = ConstantArray::get(
    			M.getContext(), "Input: %e\n", true
    	);

    	inputMsg = new GlobalVariable(
    			M,
    			input_msg->getType(),
    			true,
    			GlobalValue::InternalLinkage,
    			input_msg,
    			"inputValue"
    	);

    	Constant *float_msg = ConstantArray::get(
    	    			M.getContext(), "%f\n", true
    	);
    	floatMsg = new GlobalVariable(
    			M,
    			float_msg->getType(),
    			true,
    			GlobalValue::InternalLinkage,
    			float_msg,
    			"floatValue"
    	);

    	Constant *lf_msg = ConstantArray::get(
    			M.getContext(), "%lf\00", true
    	);
    	lfMsg = new GlobalVariable(
    			M,
    			lf_msg->getType(),
    			true,
    			GlobalValue::InternalLinkage,
    			lf_msg,
    			"lf_notation"
    	);

    	Constant *int_msg = ConstantArray::get(
    			M.getContext(), "%d\n", true
    	);
    	intMsg = new GlobalVariable (
    			M,
    			int_msg->getType(),
    			true,
    			GlobalValue::InternalLinkage,
    			int_msg,
    			"intValue"
    	);

    	mpq = dyn_cast<StructType> (M.getTypeByName("struct.__mpq_struct"));
    	mpq_Ptr = PointerType::getUnqual(mpq);

    	mpq_set_str = cast<Function>( M.getOrInsertFunction(
    			"__gmpq_set_str", int32, mpq_Ptr,
    			IntegerType::getInt8PtrTy(M.getContext()),
    			int32, NULL)
    	);

    	mpq_get_d = cast<Function>(
    			M.getOrInsertFunction(
    					"__gmpq_get_d", Type::getDoubleTy(M.getContext()
    			),
    	    	PointerType::getUnqual(mpq), NULL)
    	);
    	mpq_init = cast<Function>(
    			M.getOrInsertFunction("__gmpq_init", Type::getVoidTy(M.getContext()),
    					mpq_Ptr, NULL)
    	);

    	exit = cast <Function> (
    			M.getOrInsertFunction (
    					"exit", Type::getVoidTy(M.getContext()),
    					int32, NULL
    			)
    	);

    	FunctionType *feenableexceptType = FunctionType::get(
    			int32, true
    	);

    	feenableexcept = cast<Function>(
    			M.getOrInsertFunction("feenableexcept",	feenableexceptType)
    	);

	}

    // Declare standard function calls
    void exitCall(BasicBlock *bb) {

    	Constant *zero_32 = Constant::getNullValue(
    			IntegerType::getInt32Ty(bb->getContext()));

    	Value *exit_params[] = {
    			zero_32
    	};

    	CallInst *exit_call = CallInst::Create(
    			exit, exit_params,
    			array_endof(exit_params),"", bb
    	);
    	UnreachableInst *unreach = new UnreachableInst(bb->getContext(),bb);
    	exit_call->moveBefore(unreach);
    }

    //Print out an error message at the end of a basic block
    CallInst* printErrMsg(
    	  GlobalVariable *msg, BasicBlock *bb
    ) {

       	  Constant *zero_32 = Constant::getNullValue(
       			  IntegerType::getInt32Ty(bb->getContext()));

       	  Constant *gep_params[] = {
       			  zero_32,
       			  zero_32
       	  };

       	  Constant *errMsg = ConstantExpr::getGetElementPtr(msg, gep_params,
       	      			  array_lengthof(gep_params));

       	  Value *params[] = {
       			  errMsg
       	  };

       	  CallInst *print_call = CallInst::Create (
       			  printf, params, array_endof(params),
       			  "", bb
       	  );
       	  print_call->setTailCall(true);
       	  return print_call;
    }

    CallInst* printErrMsg(
    	  GlobalVariable *msg, Instruction *I
    ) {

           	  Constant *zero_32 = Constant::getNullValue(
           			  IntegerType::getInt32Ty(I->getContext()));

           	  Constant *gep_params[] = {
           			  zero_32,
           			  zero_32
           	  };

           	  Constant *errMsg = ConstantExpr::getGetElementPtr(msg, gep_params,
           	      			  array_lengthof(gep_params));

           	  Value *params[] = {
           			  errMsg
           	  };

           	  CallInst *print_call = CallInst::Create (
           			  printf, params, array_endof(params),
           			  "", I
           	  );
           	  print_call->setTailCall(true);
           	  return print_call;
    }

    // Debugging
    void printMessage (GlobalVariable *msg, Value *val, BasicBlock *bb) {
    	Constant *gep_params[] = {
    			zero_32,
    			zero_32
    	};

    	Constant *message = ConstantExpr::getGetElementPtr(
    			msg, gep_params,
    			array_lengthof(gep_params)
    	);

    	Value *params[] = {
    			message,
    			val
    	};

    	CallInst *print_call = CallInst::Create(
    			printf,	params, array_endof(params),
    			"", bb
    	);
    	print_call->setTailCall(true);
    }

    CallInst* printMessage (GlobalVariable *msg, Value *val, Instruction *I) {
        	Constant *gep_params[] = {
        			zero_32,
        			zero_32
        	};

        	Constant *message = ConstantExpr::getGetElementPtr(
        			msg, gep_params,
        			array_lengthof(gep_params)
        	);

        	Value *params[] = {
        			message,
        			val
        	};

        	CallInst *print_call = CallInst::Create(
        			printf,	params, array_endof(params),
        			"", I
        	);
        	print_call->setTailCall(true);
        	return print_call;
    }

    CallInst* mpq_set_str_call (
        		Value *mpq_var, Value *val, Value *base, BasicBlock *bb
    ) {
           	Value *set_params[] = {
           		mpq_var,
           		val, base
           	};

           	CallInst *mpq_set_d_call = CallInst::Create (
           			mpq_set_str, set_params, array_endof(set_params),
           			"", bb
           	);

           	return mpq_set_d_call;
    }

    CallInst* mpq_get_d_call (Value *mpq_var, BasicBlock *bb) {
        	Value *get_params[] = {
            			mpq_var
        	};

        	CallInst *mpq_get_d_call = CallInst::Create(
        			mpq_get_d, get_params, array_endof(get_params), "", bb
        	);
        	return mpq_get_d_call;
    }

    CallInst* mpq_init_call (Value *param, BasicBlock* bb){
        	Value *init_params[] = {
        			param
        	};

        	CallInst *init_call = CallInst::Create(
        			mpq_init,
        			init_params, array_endof(init_params),
        			"", bb
        	);
        	return init_call;
    }

    virtual bool runOnModule(Module &M) {

      init(M);
      std::vector<Function*> funcList;

      // Traverse a list of function in a module and print out the output
      for (Module::iterator FF = M.begin(), FE = M.end(); FF != FE; FF++){
      	  Function *function = FF;
    	  //ignore main
    	  if (
    			  function->getName().startswith("main") ||
    			  function->isDeclaration()
    	  )
    		  continue;
    	  // Assume that the module does not have a main
    	  mainFn = cast<Function> (M.getOrInsertFunction(
    			  "main", int32, int32,
    			  PointerType::getUnqual(Type::getInt8PtrTy(M.getContext())), NULL)
    	  );
    	  BasicBlock *entryBB = BasicBlock::Create(
    			  M.getContext(), "Entry", mainFn, mainFn->begin()
    	  );

    	  Function::ArgumentListType::iterator firstArg = mainFn->getArgumentList().begin();
    	  Value *argc = firstArg;
    	  argc->setName("argc");
    	  Value *argv = ++firstArg;
    	  argv->setName("argv");

    	  std::string outputFileName = "main_" + function->getName().str() + ".o";
    	  std::string ErrInfo;
    	  raw_ostream *out = new raw_fd_ostream(
    		  outputFileName.c_str(), ErrInfo, raw_fd_ostream::F_Binary
    	  );
    	  const FunctionType *fnType = function->getFunctionType();
    	  unsigned paramNum = fnType->getNumParams();
    	  std::vector < Value *> params;


    	  int numOfVars = 0;
    	  for (unsigned i = 0; i < paramNum; i++){
    		  const Type* type = fnType->getParamType(i);
    		  if (
    				  type == Type::getDoubleTy(M.getContext()) ||
    				  type == Type::getFloatTy(M.getContext())
    		  ) {
    			  numOfVars++;
    			  Constant *index = ConstantInt::get(
    					  int32, numOfVars
    			  );
    			  Instruction *elem = GetElementPtrInst::CreateInBounds(
    					  argv, index, "", entryBB
    			  );

    			  LoadInst *load_elem = new LoadInst(
    					  elem, "", entryBB
    			  );

    			  AllocaInst *mpq_alloc = new AllocaInst (mpq, "", entryBB);
    			  mpq_init_call(mpq_alloc,entryBB);
    			  mpq_set_str_call(
    					  mpq_alloc, load_elem,
    					  ConstantInt::get( IntegerType::getInt32Ty(M.getContext()), 10),
    					  entryBB
    			  );

    			  CallInst *get_d_call = mpq_get_d_call(mpq_alloc, entryBB);
    			  params.push_back(get_d_call);
    		  }
    		  else if (const PointerType *ptrTy = dyn_cast<PointerType>(type)) {
    			  AllocaInst *alloc = new AllocaInst (
    					  ptrTy->getElementType(), "", entryBB
    			  );
    			  params.push_back(alloc);
    		  }
    		  else {
    			  AllocaInst *alloc = new AllocaInst (
    					  type, "", entryBB
    			  );
    			  LoadInst *load = new LoadInst (alloc, "", entryBB);
    			  params.push_back(load);
    		  }
    	  }

    	  Value *feenable_params[] = {
    			  ConstantInt::get(int32, 29)
    	  };

    	  CallInst::Create(
    		  feenableexcept, feenable_params,
    		  array_endof(feenable_params),"", entryBB
    	  );
    	  CallInst *functionCall = CallInst::Create(
    		function, params.begin(), params.end(), "", entryBB
    	  );
    	  if (functionCall->getType() == Type::getDoubleTy(M.getContext()))
    		  printMessage(floatMsg, functionCall, entryBB);
    	  if (functionCall->getType() == Type::getInt32Ty(M.getContext()))
    		  printMessage(intMsg, functionCall, entryBB);

    	  ReturnInst::Create(M.getContext(), zero_32, entryBB);

    	  mainFn->print(errs());
    	  WriteBitcodeToFile(&M,*out);
    	  //Clean instructions in main
    	  mainFn->eraseFromParent();

      }
      M.print(errs(),&w); //debugging
      return true;
    }
  };
}

char LogLineMain::ID = 0;
static RegisterPass<LogLineMain> X("LogLineMain", "Add main to the logline transformed code");
