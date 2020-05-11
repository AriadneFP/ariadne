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
#include "llvm/Assembly/AsmAnnotationWriter.h"
#include <sstream>
#include <string>

using namespace llvm;

namespace {
  struct FP_env : public ModulePass {

	Function *klee_make_symbolic;
	Function *printf;
	Function *atof;
	Function *fetestexcept;
	Function *sscanf;
	Function *mainFn;
	Function *mpq_set_str;
	Function *mpq_init;
	Function *fpeenableexcept;
	Function *sigaction;
	Function *sigemptyset;
	Function *enable_fpe_traps;
	Function *exit;
	Function *catch_sigfpe;
	Function *feenableexcept;
	const StructType *mpq;
	const Type *mpq_Ptr;
	const Type *siginfo;
	const Type *sigactionType;
	Function *mpq_get_d;
	GlobalVariable *overflowString;
	GlobalVariable *underflowString;
	GlobalVariable *invalidString;
	GlobalVariable *divByZeroString;
	GlobalVariable *sigemptySet;
	GlobalVariable *badSigaction;
	GlobalVariable *floatMsg;
	GlobalVariable *intMsg;
	GlobalVariable *inputMsg;
	GlobalVariable *lfMsg;
	AssemblyAnnotationWriter w;
	Constant *zero_32;
	Constant *FE_OVERFLOW;
	Constant *FE_UNDERFLOW;
	Constant *FE_INVALID;
	Constant *FE_DIVBYZERO;
	ConstantInt *FPE_FLTDIV_ARIADNE;
	ConstantInt *FPE_FLTOVF_ARIADNE;
	ConstantInt *FPE_FLTUND_ARIADNE;
	ConstantInt *FPE_FLTRES_ARIADNE;
	ConstantInt *FPE_FLTINV_ARIADNE;
	const Type *int32;

    static char ID; // Pass identification, replacement for typeid
    FP_env() : ModulePass(&ID) {}

    void init(Module &M) {
    	Constant *overFlowMsg = ConstantArray::get(
    			M.getContext(), "Overflow!\n", true
    	);

    	overflowString = new GlobalVariable(
    			M,
    			overFlowMsg->getType(),
    			true,
    			GlobalValue::InternalLinkage,
    			overFlowMsg,
    			"overflow"
    	);

    	Constant *underflowMsg = ConstantArray::get(
    			M.getContext(), "Underflow!\n", true
    	);

    	underflowString = new GlobalVariable(
    			M,
    			underflowMsg->getType(),
    			true,
    			GlobalValue::InternalLinkage,
    			underflowMsg,
    			"underflow"
    	);

    	Constant *invalidMsg = ConstantArray::get(
    			M.getContext(), "Invalid!\n", true
    	);

    	invalidString = new GlobalVariable(
    			M,
    			invalidMsg->getType(),
    			true,
    			GlobalValue::InternalLinkage,
    			invalidMsg,
    			"invalid"
    	);

    	Constant *divByZeroMsg = ConstantArray::get(
    			M.getContext(), "Divide by Zero!\n", true
    	);

    	divByZeroString = new GlobalVariable(
    			M,
    			divByZeroMsg->getType(),
    			true,
    			GlobalValue::InternalLinkage,
    			divByZeroMsg,
    			"DivbyZero"
    	);

    	Constant *sigemptysetMsg = ConstantArray::get(
    			M.getContext(), "sigemptyset!\n", true
    	);

    	sigemptySet = new GlobalVariable(
    			M,
    			sigemptysetMsg->getType(),
    			true,
    			GlobalValue::InternalLinkage,
    			sigemptysetMsg,
    			"sigemptyset"
    	);

    	Constant *badsigactionMsg = ConstantArray::get(
    			M.getContext(), "Bad sigaction call!\n", true
    	);

    	badSigaction = new GlobalVariable(
    			M,
    			badsigactionMsg->getType(),
    			true,
    			GlobalValue::InternalLinkage,
    			badsigactionMsg,
    			"badSigaction"
    	);

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

    	atof = cast<Function>(M.getOrInsertFunction(
    			"atof",
    			Type::getDoubleTy(M.getContext()),
    			Type::getInt8PtrTy(M.getContext()),
    			NULL)
    	);

    	fetestexcept = cast<Function>(M.getOrInsertFunction(
    			"fetestexcept",
    			Type::getInt32Ty(M.getContext()),
    			Type::getInt32Ty(M.getContext()),
    			NULL)
    	);

    	//Declare sscanf function
    	std::vector<const Type*> ft_sscanf_args;
    	ft_sscanf_args.push_back(
    			PointerType::getUnqual(IntegerType::getInt8Ty(M.getContext()))
    	);
    	ft_sscanf_args.push_back(
    	    	PointerType::getUnqual(IntegerType::getInt8Ty(M.getContext()))
    	);
    	FunctionType *ft_sscanf = FunctionType::get(
    			IntegerType::getInt32Ty(M.getContext()), ft_sscanf_args, true
    	);

    	sscanf = cast<Function>(
    			M.getOrInsertFunction("\01__isoc99_sscanf",	ft_sscanf)
    	);

    	zero_32 = Constant::getNullValue(
    			IntegerType::getInt32Ty(M.getContext())
    	);

    	FE_OVERFLOW = ConstantInt::get(
    			Type::getInt32Ty(M.getContext()), 8
    	);

    	FE_UNDERFLOW = ConstantInt::get(
    			Type::getInt32Ty(M.getContext()), 16
    	);

    	FE_INVALID = ConstantInt::get(
    			Type::getInt32Ty(M.getContext()), 1
    	);
    	FE_DIVBYZERO = ConstantInt::get(
    			Type::getInt32Ty(M.getContext()), 4
    	);

    	FPE_FLTDIV_ARIADNE = ConstantInt::get(
    			Type::getInt32Ty(M.getContext()), 3
    	);
    	FPE_FLTOVF_ARIADNE = ConstantInt::get(
    			Type::getInt32Ty(M.getContext()), 4
    	);
    	FPE_FLTUND_ARIADNE = ConstantInt::get(
    			Type::getInt32Ty(M.getContext()), 5
    	);
    	FPE_FLTRES_ARIADNE = ConstantInt::get(
    			Type::getInt32Ty(M.getContext()), 6
    	);
    	FPE_FLTINV_ARIADNE = ConstantInt::get(
    			Type::getInt32Ty(M.getContext()), 7
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
    	sigactionType = dyn_cast<StructType> (M.getTypeByName("struct.sigaction"));
    	siginfo = dyn_cast<StructType> (M.getTypeByName("struct.siginfo_t"));
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

    	enable_fpe_traps = cast <Function> (
    			M.getOrInsertFunction("enable_fpe_traps", Type::getVoidTy(M.getContext()),
    					NULL)
    	);

    	catch_sigfpe = cast <Function> (
    			M.getOrInsertFunction("catch_sigfpe", Type::getVoidTy(M.getContext()),
    					int32, PointerType::getUnqual(siginfo),
    					Type::getInt8PtrTy(M.getContext()), NULL)
    	);

    	exit = cast <Function> (
    			M.getOrInsertFunction("exit", Type::getVoidTy(M.getContext()),
    					int32,
    					NULL)
    	);

    	sigaction = cast <Function> (
    			M.getOrInsertFunction("sigaction", int32, int32,
    					PointerType::getUnqual(sigactionType),
    					PointerType::getUnqual(sigactionType), NULL
    			)
    	);

    	FunctionType *feenableexceptType = FunctionType::get(
    			int32, true
    	);

    	feenableexcept = cast<Function>(
    			M.getOrInsertFunction("feenableexcept",	feenableexceptType)
    	);

    	defineCatch_Sigfpe(M);
    	defineEnable_Fpe_traps(M);

	}

    // Declare standard function calls
    void exitCall(BasicBlock *bb) {

    	Constant *zero_32 = Constant::getNullValue(
    			IntegerType::getInt32Ty(bb->getContext()));

    	Value *exit_params[] = {
    			zero_32
    	};

    	CallInst *exit_call =
    			CallInst::Create(exit, exit_params,
    					array_endof(exit_params),"", bb);
    	UnreachableInst *unreach = new UnreachableInst(bb->getContext(),bb);
    	exit_call->moveBefore(unreach);
    }


    void defineCatch_Sigfpe(Module &M) {
    	if (!catch_sigfpe->isDeclaration())
    		return;
    	Function::ArgumentListType::iterator it = catch_sigfpe->getArgumentList().begin();
    	Value *sig = it;
    	//Set name for parameter
    	sig->setName("sig");
    	Value *code = ++it;
    	code->setName("code");
    	Value *v = ++it;
    	v->setName("v");
    	BasicBlock *entryBB = BasicBlock::Create(
    			M.getContext(), "entry", catch_sigfpe
    	);

    	Value *idx[] = {
    			ConstantInt::get(
    					Type::getInt32Ty(M.getContext()), 0
    	    						),
    	    	ConstantInt::get(
    	    			Type::getInt32Ty(M.getContext()), 2
    	    	)
    	};
    	GetElementPtrInst *element = GetElementPtrInst::CreateInBounds (
    			code, idx, idx+2, "", entryBB
    	);

    	LoadInst *load_elem = new LoadInst(element, "", entryBB);

    	BasicBlock *overflowPrintBB = BasicBlock::Create(
    			M.getContext(), "Overflow", catch_sigfpe
    	);
    	BasicBlock *underflowPrintBB = BasicBlock::Create(
    			M.getContext(), "Underflow", catch_sigfpe
    	);
    	BasicBlock *invalidPrintBB = BasicBlock::Create(
    			M.getContext(), "Invalid", catch_sigfpe
    	);
    	BasicBlock *divByZeroPrintBB = BasicBlock::Create(
    			M.getContext(), "DivByZero", catch_sigfpe
    	);

    	BasicBlock *exitBB = BasicBlock::Create(
    			M.getContext(), "default", catch_sigfpe
    	);

    	printErrMsg(overflowString, overflowPrintBB);
    	printErrMsg(underflowString, underflowPrintBB);
    	printErrMsg(invalidString, invalidPrintBB);
    	printErrMsg(divByZeroString, divByZeroPrintBB);

    	SwitchInst *switchInstruction = SwitchInst::Create(
    			load_elem, exitBB, 4, entryBB
    	);

    	switchInstruction->addCase(FPE_FLTDIV_ARIADNE, divByZeroPrintBB);
    	switchInstruction->addCase(FPE_FLTINV_ARIADNE, invalidPrintBB);
    	switchInstruction->addCase(FPE_FLTOVF_ARIADNE, overflowPrintBB);
    	switchInstruction->addCase(FPE_FLTUND_ARIADNE, underflowPrintBB);

    	BranchInst::Create(exitBB, underflowPrintBB);
    	BranchInst::Create(exitBB, overflowPrintBB);
    	BranchInst::Create(exitBB, divByZeroPrintBB);
    	BranchInst::Create(exitBB, invalidPrintBB);
    	exitCall(exitBB);

    }

    void defineEnable_Fpe_traps(Module &M) {

    	BasicBlock *entryBB = BasicBlock::Create(
    			M.getContext(), "entry", enable_fpe_traps
    	);

    	AllocaInst *act = new AllocaInst(sigactionType, "act", entryBB);
    	Value *idx[] = {
    			ConstantInt::get(
    					Type::getInt32Ty(M.getContext()), 0
    	    	    						),
    	    	ConstantInt::get(
    	    			Type::getInt32Ty(M.getContext()), 0
    	    	)
    	};
    	GetElementPtrInst *element = GetElementPtrInst::CreateInBounds (
    			act, idx, idx+2, "", entryBB
    	);

    	GetElementPtrInst *sig_Elem = GetElementPtrInst::CreateInBounds (
    			element, idx, idx+2, "", entryBB
    	);

    	//const Type *sigfpeType = dyn_cast<Type>(catch_sigfpe->getFunctionType());
    	BitCastInst *funcBitCast = new BitCastInst(
    			sig_Elem, PointerType::getUnqual(catch_sigfpe->getType()),
    			"", entryBB
    	);
    	new StoreInst(catch_sigfpe, funcBitCast, entryBB);

    	Value *idx2[] = {
    			ConstantInt::get(
    					Type::getInt32Ty(M.getContext()), 0
    	    	    	    						),
    	    	ConstantInt::get(
    	    			Type::getInt32Ty(M.getContext()), 2
    	    	)
    	};

    	GetElementPtrInst *sa_flags = GetElementPtrInst::CreateInBounds (
    			act, idx2, idx2+2, "sa_flags", entryBB
    	);

    	new StoreInst(ConstantInt::get(int32, 4), sa_flags, entryBB);

    	Constant *zero_32 = Constant::getNullValue(
    	    			IntegerType::getInt32Ty(M.getContext()));

    	Value *sigaction_params[] = {
    			ConstantInt::get(int32, 8),
    			act,
    			Constant::getNullValue(PointerType::getUnqual(sigactionType))
    	};

    	CallInst *sigactionCall = CallInst::Create(sigaction, sigaction_params,
				array_endof(sigaction_params),"", entryBB);

    	BasicBlock *printBadSigAction = BasicBlock::Create(
    			M.getContext(), "printBadSigAction", enable_fpe_traps
    	);

    	BasicBlock *returnBB = BasicBlock::Create(
    			M.getContext(), "return", enable_fpe_traps
    	);

    	CmpInst *sigCallNeZero = CmpInst::Create(
    			Instruction::ICmp, FCmpInst::ICMP_NE,
    			sigactionCall, zero_32, "", entryBB
    	);

    	BranchInst::Create (
    			printBadSigAction, returnBB, sigCallNeZero, entryBB
    	);

    	printErrMsg(badSigaction, printBadSigAction);
    	BranchInst::Create(returnBB, printBadSigAction);

	/// Value 29 represents for the combination of four types of exceptions:
	/// FE_OVERFLOW, FE_UNDERFLOW, FE_DIVBYZERO, FE_INVALID
    	Value *feenable_params[] = {
    			ConstantInt::get(int32, 29)
    	};
    	/*Value *feenable_params[] = {
    			FE_DIVBYZERO
    	};*/

    	CallInst::Create(
    			feenableexcept, feenable_params,
    			array_endof(feenable_params),"", returnBB
    	);

    	ReturnInst::Create(M.getContext(), returnBB);
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


    CallInst* atof_call (Value *var, BasicBlock *bb){
        	Value *params[] = {
        			var
        	};

        	CallInst *atof_call = CallInst::Create(
        			atof, params, array_endof(params), "", bb
        	);
        	return atof_call;
    }

    CallInst* sscanf_call (Value *arg, Value *var_addr, BasicBlock *bb){
			Constant *gep_params[] = {
					zero_32,
					zero_32
			};

			Constant *msg = ConstantExpr::getGetElementPtr(lfMsg, gep_params,
    	       	      			  array_lengthof(gep_params));

		   Value *params[] = {arg, msg, var_addr};

           CallInst *sscanf_call = CallInst::Create(
           		sscanf, params, array_endof(params), "", bb
           );
           return sscanf_call;
    }

    CallInst* fetestexcept_call (Value *var, BasicBlock *bb){
			Value *params[] = {
            			var
			};

			CallInst *fetestexcept_call = CallInst::Create(
					fetestexcept, params, array_endof(params), "", bb
			);
			return fetestexcept_call;
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

    // Debugging: print all values of instructions in a basic block
    void printValues(BasicBlock &B) {
    	for(BasicBlock::iterator II = B.begin(), IE = B.end(); II != IE; II++){
    		Instruction *I = II;
    		if (I->getType() == Type::getDoubleTy(B.getContext())) {
    			Module *M = B.getParent()->getParent();
    			Constant *msg = ConstantArray::get(
    					M->getContext(), "name\n", true
    			);
    			GlobalVariable *Msg = new GlobalVariable(
    					*M,
						msg->getType(),
						true,
						GlobalValue::InternalLinkage,
						msg,
						""
    			);
    			CallInst *printName = printErrMsg(Msg, I);
    			I->moveBefore(printName);
    			CallInst *print = printMessage(floatMsg, I, I);
    			I->moveBefore(print);
    		}
    	}
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

    void runOnFunction(Function &F){

    	StringRef name = F.getName();
	// Ignore some functions: fabs, fabsf
    	if (name.compare("fabs") == 0 || name.compare("fabsf") == 0)
    		return;
    	for (Function::iterator BB = F.begin(), BE = F.end(); BB != BE; BB++) {
    		BasicBlock *B = BB;
    		printValues(*B);
    	}
    }


    virtual bool runOnModule(Module &M) {

      init(M);
      std::vector<Function*> funcList;
      // Traverse a list of function in a module and print out the output
      for (Module::iterator FF = M.begin(), FE = M.end(); FF != FE; FF++){
		  Function *function = FF;
		  /// Ignore main, we do not transform main function
    	  if ( function->getName().startswith("main") ||
    		   function->getName().startswith("enable_fpe_traps") ||
    		   function->getName().startswith("catch_sigfpe") ||
    		   function->isDeclaration())
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
    	  CallInst::Create (enable_fpe_traps, "", entryBB);
    	  CallInst *functionCall = CallInst::Create(
		  function, params.begin(), params.end(), "", entryBB
    	  );
    	  if (functionCall->getType() == Type::getDoubleTy(M.getContext()))
    		  printMessage(floatMsg, functionCall, entryBB);
    	  if (functionCall->getType() == Type::getInt32Ty(M.getContext()))
    		  printMessage(intMsg, functionCall, entryBB);

    	  ReturnInst::Create(M.getContext(), zero_32, entryBB);

    	  mainFn->print(errs());
    	  // Remove the function from the module to let the function call the
    	  // library
    	  //function->deleteBody();
    	  WriteBitcodeToFile(&M,*out);
    	  //Clean instructions in main
    	  mainFn->eraseFromParent();

      }
      M.print(errs(),&w); //debugging
      return true;
    }
  };
}

char FP_env::ID = 0;
static RegisterPass<FP_env> X("FP_env", "FP environment Pass");
