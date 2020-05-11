/*
 * FunctionCall.cpp
 *
 * Created on: Feb 15, 2011
 * Author: thanh
 *
 * Inject function call in main function of a module.
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
#include <sstream>
#include <string>
#include <fstream>

using namespace llvm;
using namespace std;

namespace {

  struct FunctionCall : public ModulePass {

	const StructType *mpq;
	const Type *mpq_Ptr;
	Function *mpq_set_str;
	Function *mpq_init;
	Function *mpq_get_d;
	Function *feenableexcept;
	Function *fedisableexcept;
	Function *exit;
	Function *printf;

	const Type *int32;
	Constant *zero_32;

	ifstream inpFile;
	// Enable exceptions with FENV.
	bool FENV;
	static char ID; // Pass identification, replacement for typeid
    FunctionCall() : ModulePass(&ID) {}

    enum OutputType {
    	IR,
    	SourceC,
    	Both
    };

    OutputType outputType;

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

    CallInst* mpq_set_str_call (
    	Value *mpq_var, Value *val, Value *base, Instruction *inst
    ) {
        	Value *set_params[] = {
        			mpq_var,
        			val, base
        	};

        	CallInst *mpq_set_d_call = CallInst::Create (
        			mpq_set_str, set_params, array_endof(set_params),
        			"", inst
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

    CallInst* mpq_get_d_call(Value *mpq_var, Instruction *inst) {
		Value *get_params[] = {
        			mpq_var
        	};

		CallInst *mpq_get_d_call = CallInst::Create(
				mpq_get_d, get_params, array_endof(get_params), "", inst
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

    CallInst* mpq_init_call (Value *param, Instruction* inst){
       	Value *init_params[] = {
      			param
       	};

       	CallInst *init_call = CallInst::Create(
       			mpq_init,
       			init_params, array_endof(init_params),
       			"", inst
    	);
       	return init_call;
    }

    //Print out an error message at the end of a basic block
    void printErrMsg (GlobalVariable *msg, BasicBlock *bb) {
		Constant *zero_32 = Constant::getNullValue(IntegerType::getInt32Ty(
				bb->getContext()));

		Constant *gep_params[] = { zero_32, zero_32 };

		Constant *errMsg = ConstantExpr::getGetElementPtr(msg, gep_params,
				array_lengthof(gep_params));

		Value *params[] = { errMsg };

		CallInst *print_call = CallInst::Create(printf, params, array_endof(
				params), "", bb);
		print_call->setTailCall(true);

		Value *exit_params[] = { zero_32 };

		CallInst *exit_call = CallInst::Create(exit, exit_params, array_endof(
				exit_params), "", bb);
		UnreachableInst *unreach = new UnreachableInst(bb->getContext(), bb);
		exit_call->moveBefore(unreach);
	}

    void printMsg (GlobalVariable *msg, Instruction *nextInst) {
    		Constant *zero_32 = Constant::getNullValue(IntegerType::getInt32Ty(
    				nextInst->getContext()));

    		Constant *gep_params[] = { zero_32, zero_32 };

    		Constant *errMsg = ConstantExpr::getGetElementPtr(msg, gep_params,
    				array_lengthof(gep_params));

    		Value *params[] = { errMsg };

    		CallInst *print_call = CallInst::Create(printf, params, array_endof(
    				params), "", nextInst);
    		print_call->setTailCall(true);
    }

    void printMsg (GlobalVariable *msg, BasicBlock *bb) {
       		Constant *zero_32 = Constant::getNullValue(IntegerType::getInt32Ty(
     			bb->getContext()));

       		Constant *gep_params[] = { zero_32, zero_32 };

        	Constant *errMsg = ConstantExpr::getGetElementPtr(msg, gep_params,
        				array_lengthof(gep_params));

       		Value *params[] = { errMsg };

       		CallInst *print_call = CallInst::Create(printf, params, array_endof(
       				params), "", bb);
       		print_call->setTailCall(true);
    }

    AllocaInst* allocPointerValue(
    	const PointerType *ptrType, Instruction *nextInst
    ) {
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
        		double x;
        		inpFile >> x;
        		Constant *constant = ConstantFP::get(orgType,x);
        		new StoreInst(constant, alloc, nextInst);
        	} else if (
        		orgType->isIntegerTy()
        	) {
        		int val;
        		inpFile >> val;
        		Constant *constant = ConstantInt::get(orgType, val);
        		new StoreInst(constant, alloc, nextInst);
        	}
        }
        return alloc;
    }

    virtual bool doInitialization(Module &M){

    	// Set the normal mode by getting the value of environment variable __ARIADNE_NORMAL
    	int fenv = atoi(getenv("__ARIADNE_FENV"));
    	if (fenv == 1)
    		FENV = true;
    	else
    		FENV = false;

    	int32 = Type::getInt32Ty(M.getContext());
    	zero_32 = Constant::getNullValue(
    			IntegerType::getInt32Ty(M.getContext())
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
    			M.getOrInsertFunction(
    					"__gmpq_init", Type::getVoidTy(M.getContext()),
    					mpq_Ptr, NULL
    			)
    	);

    	FunctionType *feenableexceptType = FunctionType::get(
    			int32, true
    	);

    	FunctionType *fedisableexceptType = FunctionType::get(
    			int32, true
    	);

    	feenableexcept = cast<Function>(
    			M.getOrInsertFunction("feenableexcept",	feenableexceptType)
    	);

    	fedisableexcept = cast<Function>(
    			M.getOrInsertFunction("fedisableexcept", fedisableexceptType)
    	);

    	//Declare exit function
    	exit = cast<Function>(M.getOrInsertFunction(
		  "exit", Type::getVoidTy(M.getContext()),
		  Type::getInt32Ty(M.getContext()), NULL)
    	);

    	//Declare printf function
    	vector<const Type*> ft_printf_args;
		ft_printf_args.push_back(PointerType::getUnqual(IntegerType::getInt8Ty(
				M.getContext())));

		FunctionType* ft_printf = FunctionType::get(IntegerType::getInt32Ty(
				M.getContext()), ft_printf_args, true);

		printf = cast<Function> (M.getOrInsertFunction("printf", ft_printf));

		outputType = SourceC;

    	return true;
    }

    unsigned getFloatingPointParams(const FunctionType* fnType) {
    	/// Count number of floating point parameters
    	int numOfParams = 0;
    	for (unsigned i = 0; i < fnType->getNumParams(); i++) {
    		const Type* type = fnType->getParamType(i);
    		if (
    			type == Type::getDoubleTy(fnType->getContext()) ||
    			type == Type::getFloatTy(fnType->getContext())
    		)
    		  numOfParams++;
    	}
    	return numOfParams;
    }

    void createParamCheck(
    		Module &M, Function *mainFn, Function* function,
    		BasicBlock* entryBB, BasicBlock* warningBB,
    		BasicBlock* functionCallBB,	Value* argc, Value* argv
    ) {
    	const FunctionType *fnType = function->getFunctionType();
    	unsigned numOfParams = getFloatingPointParams(fnType);
    	Constant *numOfParamsConst = ConstantInt::get(int32, numOfParams);

    	Value *fenv_params[] = {
    	  	  ConstantInt::get(int32, 29)
    	};

    	CallInst::Create(
    		  feenableexcept, fenv_params,
    	  	  array_endof(fenv_params),"", entryBB
    	);

    	CmpInst* sleOp = CmpInst::Create(
    		  Instruction::ICmp, ICmpInst::ICMP_SLE,
    		  argc, numOfParamsConst, "", entryBB
    	);

        BranchInst::Create(
    		  warningBB, functionCallBB,
    		  sleOp->getUnderlyingObject(), entryBB
    	);

        string warningStr =
        	"The number of arguments less than the number of parameters\n";

        Constant *warningMsg = ConstantArray::get(
    		  M.getContext(), warningStr.c_str(), true
    	);

        GlobalVariable *warningString = new GlobalVariable(
        	  M,
    		  warningMsg->getType(), true, GlobalValue::InternalLinkage,
    		  warningMsg, "warning"
        );

    	printErrMsg(warningString, warningBB);
    }

    raw_ostream* createOutputBytecode(Function* function) {
    	// Put all bitcodes generating executables in EXE directory
    	string outputFileName;
    	if (!FENV)
    	   outputFileName = "EXE/main_" + function->getName().str() + ".o";
    	else
    	   outputFileName = "main_" + function->getName().str() + ".o";

    	string inpFileName = function->getName().str() + ".rand";
    	inpFile.open(inpFileName.c_str(), ifstream::in);
    	string ErrInfo;
    	return (new raw_fd_ostream(
    	  outputFileName.c_str(), ErrInfo, raw_fd_ostream::F_Binary
    	) );
    }

    void writeFunctionCallIR(
    	Function* function, Value* argv,
    	BasicBlock* functionCallBB
    ) {
    	const FunctionType* fnType = function->getFunctionType();
    	Instruction *nextInst = ReturnInst::Create(
    		  fnType->getContext(), zero_32, functionCallBB
    	);
    	unsigned paramNum = fnType->getNumParams();
    	vector<Value *> params;
		int index = 0;
		for (unsigned i = 0; i < paramNum; i++) {
			const Type* type = fnType->getParamType(i);
			if (type == Type::getDoubleTy(fnType->getContext()) || type
					== Type::getFloatTy(fnType->getContext())) {
				index++;
				Constant *indexConst = ConstantInt::get(int32, index);
				Instruction *elem = GetElementPtrInst::CreateInBounds(argv,
						indexConst, "", nextInst);

				LoadInst *load_elem = new LoadInst(elem, "", nextInst);

				AllocaInst *mpq_alloc = new AllocaInst(mpq, "", nextInst);
				mpq_init_call(mpq_alloc, nextInst);
				mpq_set_str_call(mpq_alloc, load_elem, ConstantInt::get(
						IntegerType::getInt32Ty(fnType->getContext()), 10), nextInst);

				CallInst *get_d_call = mpq_get_d_call(mpq_alloc, nextInst);
				params.push_back(get_d_call);
			}

			else if (
				const PointerType *ptrType = dyn_cast<PointerType> (type)
    	    ) {
				AllocaInst *alloc = allocPointerValue(ptrType, nextInst);
				params.push_back(alloc);
			} else if (type->isIntegerTy()) {
				int val;
				inpFile >> val;
				Constant *constant = ConstantInt::get(type, val);
				params.push_back(constant);
			} else {
				AllocaInst *alloc = new AllocaInst(type, "", nextInst);
				LoadInst *load = new LoadInst(alloc, "", nextInst);
				params.push_back(load);
			}
		}
		if (FENV) {
			Value *feenable_params[] = { ConstantInt::get(int32, 29) };

			CallInst::Create(feenableexcept, feenable_params, array_endof(
					feenable_params), "", nextInst);
		}

		CallInst* function_Call = CallInst::Create(function, params.begin(),
				params.end(), "", nextInst);
		function_Call->setCallingConv(function->getCallingConv());

		Value *fenv_params[] = {
			ConstantInt::get(int32, 29)
		};

		CallInst::Create(
			fedisableexcept, fenv_params,
		    array_endof(fenv_params),"", function_Call
		);
    }

    void writeMainFunctionIR(Module& M, Function* function) {
    	Function *mainFn = cast<Function> (M.getOrInsertFunction("main", int32,
				int32, PointerType::getUnqual(
						Type::getInt8PtrTy(M.getContext())), NULL
		));

		/// Get function parameters
		Function::ArgumentListType::iterator firstArg =
				mainFn->getArgumentList().begin();
		Value *argc = firstArg;
		argc->setName("argc");
		Value *argv = ++firstArg;
		argv->setName("argv");

		BasicBlock *entryBB = BasicBlock::Create(M.getContext(), "Entry",
				mainFn, mainFn->begin());

		BasicBlock *warningBB = BasicBlock::Create(M.getContext(), "Warning",
				mainFn);

		BasicBlock *functionCallBB = BasicBlock::Create(M.getContext(),
				"Function_Call", mainFn);

		createParamCheck(M, mainFn, function, entryBB, warningBB,
				functionCallBB, argc, argv);

		writeFunctionCallIR(function, argv, functionCallBB);

		/// Export the bytecodes
		WriteBitcodeToFile(&M, *createOutputBytecode(function));
		inpFile.close();

		//Clean instructions in main
		mainFn->eraseFromParent();
    }

    void writeMainPreambleSourceC(
    	ofstream& outCFile
    ) {
    	outCFile << "#include <fenv.h>" << endl;
    	outCFile << "#include <gmp.h>" << endl;
    	outCFile << "#include <gsl/gsl_specfunc.h>" << endl;
    	outCFile << "int main(int argc, char* argv[]) {" << endl;
    	outCFile << "\tfeenableexcept(29);" << endl;
    	outCFile << "\tint paramIndex = 0;" << endl;
    	outCFile << "\tdouble* argvParams = new double[argc];" << endl;
    }

    void writeFunctionCallSourceC(Function* function, ofstream& outCFile) {
    	string inpFileName = function->getName().str() + ".rand";
    	inpFile.open(inpFileName.c_str(), ifstream::in);
    	string functionCall = "\t" + string(function->getName());
    	functionCall += "(";
    	const FunctionType* fnType = function->getFunctionType();
    	int paramIndex = 0;
    	for (unsigned i = 0; i < fnType->getNumParams(); i++) {
			const Type* type = fnType->getParamType(i);
			if (type == Type::getDoubleTy(fnType->getContext()) ||
				type == Type::getFloatTy(fnType->getContext())) {
				functionCall += "argvParams[";
				stringstream ss;
				ss << paramIndex;
				functionCall += ss.str();
				functionCall += "]";
				paramIndex++;
			}
			/*else if (
    			const PointerType *ptrType = dyn_cast<PointerType> (type)
    	    ) {

			}*/ else if (type->isIntegerTy()) {
				int val;
				inpFile >> val;
				stringstream ss;
				ss << val;
				functionCall += ss.str();
			} else {

			}
			if (i < fnType->getNumParams() -1 )
				functionCall += ",";
		}
    	functionCall += ");\n";
    	outCFile << functionCall;
    }

    void writeMainFunctionSourceC(Function* function, ofstream& outCFile) {
    	string forStr = "\tfor(int i = 0; i < ";
    	stringstream ss;
    	ss << function->getFunctionType()->getNumParams();
    	forStr += ss.str();
    	forStr += "; i++){\n";
    	outCFile << forStr;
    	outCFile << "\t\tmpq_t tmp;\n";
    	outCFile << "\t\tmpq_init( tmp );\n";
    	outCFile << "\t\tmpq_set_str( tmp, argv[paramIndex], 10 );\n";
    	outCFile << "\t\targvParams[paramIndex] = mpq_get_d( tmp );\n";
    	outCFile << "\t\tparamIndex++;\n";
    	outCFile << "\t}\n";
    	writeFunctionCallSourceC(function, outCFile);
    }

    void writeMainEpiloqueSourceC(ofstream& outCFile) {
    	outCFile << "\tfedisableexcept(29);" << endl;
    	outCFile << "}" << endl;
    	inpFile.close();
    }

    void writeMainFunctionSourceC(Module& M, Function* function) {
    	string outputFileName = "EXE/main_" + function->getName().str() + ".c";
    	ofstream outCFile(outputFileName.c_str());
    	writeMainPreambleSourceC(outCFile);
    	writeMainFunctionSourceC(function, outCFile);
    	writeMainEpiloqueSourceC(outCFile);
    }

    virtual bool runOnModule(Module &M) {
      doInitialization(M);
      // Traverse a list of function in a module and print out the output
      for (Module::iterator FF = M.begin(), FE = M.end(); FF != FE; FF++){
    	  Function *function = FF;
    	  // Ignore some functions
    	  if (function->isIntrinsic() || function->isDeclaration() ||
    		  function->getName().compare("fabs") == 0 ||
    		  function->getName().compare("fabsf") == 0 )
    		  continue;
    	  switch (outputType) {
			  case IR:
				  writeMainFunctionIR(M, function);
				  break;
			  case SourceC:
				  writeMainFunctionSourceC(M, function);
				  break;
			  case Both:
				  writeMainFunctionIR(M, function);
				  writeMainFunctionSourceC(M, function);
				  break;
		  }
      }
      return true;
    }
  };
}

char FunctionCall::ID = 0;
static RegisterPass<FunctionCall> X("FunctionCall", "Add function call in main");
