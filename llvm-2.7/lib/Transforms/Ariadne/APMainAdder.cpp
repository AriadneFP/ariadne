/*
 * This transformation will add a main function to a program within AP transformation
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
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Assembly/AsmAnnotationWriter.h"
#include <float.h>
#include <sstream>
#include <string>

using namespace llvm;

namespace {
  struct APMainAdder : public ModulePass {
	AssemblyAnnotationWriter w; //debugging
	Function *mainFn;
	Function *setConstants;
	Module *APMain;
	const StructType *mpq;
	const StructType *mpq_gsl_result;
	const Type* mpq_gsl_result_ptr;
	const ArrayType *mpqArray;
	const Type *mpq_Ptr;
	const Type *mpqArray_Ptr;
	const Type *int32;
	const Type *argvType;
	Function *mpq_init;
	Function *mpq_clear;
	Function *mpq_set_d;
	Function *mpq_get_d;
	Function *mpq_set;
	Function *mpq_set_str;
	Function *mpq_cmp;
	Function *printf;
	GlobalVariable *no_Argument;
	GlobalVariable *floatMsg;
	GlobalVariable *intMsg;
	Constant *zero_32;
	Constant *one_32;
	Constant *mone_32;

    static char ID; // Pass identification, replacement for typeid
    APMainAdder() : ModulePass(&ID) {}

    /* Build the gmp function calls with parameters and basic blocks*/
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

    CallInst* mpq_clear_call (Value *param, BasicBlock* bb){
        	Value *init_params[] = {
        			param
        	};

        	CallInst *init_call = CallInst::Create(
        			mpq_clear,
        			init_params, array_endof(init_params),
        			"", bb
        	);
        	return init_call;
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

    CallInst* mpq_get_d_call (Value *mpq_var, BasicBlock *bb){
    	Value *get_params[] = {
        			mpq_var
    	};

    	CallInst *mpq_get_d_call = CallInst::Create(
    			mpq_get_d, get_params, array_endof(get_params), "", bb
    	);
    	return mpq_get_d_call;
    }

    CallInst* mpq_get_d_call (Value *mpq_var, Instruction* nextInst){
    	Value *get_params[] = {
    			mpq_var
    	};

    	CallInst *mpq_get_d_call = CallInst::Create(
    		mpq_get_d, get_params, array_endof(get_params), "", nextInst
    	);
        return mpq_get_d_call;
    }

    CallInst* mpq_set_d_call (Value *mpq_var, Value *val, Instruction *inst){
    	Value *set_params[] = {
    			mpq_var,
        		val
    	};

    	CallInst *mpq_set_d_call = CallInst::Create(
    			mpq_set_d, set_params, array_endof(set_params),
    			"", inst
    	);
    	return mpq_set_d_call;
    }

    CallInst* mpq_set_call (Value *mpq_var, Value *val, BasicBlock *bb){
    	Value *set_params[] = {
    			mpq_var,
    			val
    	};

    	CallInst *mpq_set_call = CallInst::Create(
    			mpq_set, set_params, array_endof(set_params),
    			"", bb
    	);
    	return mpq_set_call;
    }

    CallInst* mpq_set_call (Value *mpq_var, Value *val, Instruction *inst){
       	Value *set_params[] = {
       			mpq_var,
       			val
       	};

       	CallInst *mpq_set_call = CallInst::Create(
       			mpq_set, set_params, array_endof(set_params),
       			"", inst
       	);
       	return mpq_set_call;
    }

    CallInst* mpq_cmp_call (Value *op1, Value *op2, Instruction *inst) {
    	Value *cmp_params[] = {
    			op1, op2
    	};

    	CallInst *mpq_cmp_call = CallInst::Create(
    			mpq_cmp, cmp_params, array_endof(cmp_params),"", inst
    	);
    	return mpq_cmp_call;
    }

    void checkValue(
    	Value* value, std::vector < Instruction *> addedInsts,
    	Instruction *ret_addr, Instruction *load_return,
    	Module &M, raw_ostream *out
    ) {
    	/*
    	 *  Set return value as default -1, change to 1 if the result is
    	 *  interesting one
    	 */

    	StoreInst *zero_Store = new StoreInst (
    		mone_32, ret_addr, false, 8, load_return
    	);
    	addedInsts.push_back(zero_Store);
		AllocaInst *val_addr = new AllocaInst (mpq, "", load_return);
		addedInsts.push_back(val_addr);

		CallInst *val_init_call = mpq_init_call(val_addr,load_return);
		addedInsts.push_back(val_init_call);

		StoreInst *tmpStore = new StoreInst (
			value, val_addr, false, 8, load_return
		);

		addedInsts.push_back(tmpStore);

		// Declare boundary values
		Constant *max = ConstantFP::get (
			Type::getDoubleTy(value->getContext()), DBL_MAX
		);
		Constant *min = ConstantFP::get (
			Type::getDoubleTy(value->getContext()), DBL_MIN
		);

		Constant *nmax = ConstantFP::get (
			Type::getDoubleTy(value->getContext()), -DBL_MAX
		);

		Constant *nmin = ConstantFP::get (
			Type::getDoubleTy(value->getContext()), -DBL_MIN
		);

		Constant *zero_fp = ConstantFP::get (
			Type::getDoubleTy(value->getContext()), 0
		);

		// Declare addresses for gmp boundary values
    	// Set value for gmp max
    	AllocaInst *max_addr = new AllocaInst (mpq, "", load_return);
    	addedInsts.push_back(max_addr);

		CallInst *max_init_call = mpq_init_call(max_addr,load_return);
		addedInsts.push_back(max_init_call);
		CallInst *max_set_call = mpq_set_d_call(max_addr, max, load_return);
    	addedInsts.push_back(max_set_call);

		// Set value for gmp -max
		AllocaInst *nmax_addr = new AllocaInst (mpq, "", load_return);
		addedInsts.push_back(nmax_addr);
		CallInst *nmax_init_call = mpq_init_call(nmax_addr,load_return);
		addedInsts.push_back(nmax_init_call);
		CallInst *nmax_set_call = mpq_set_d_call(nmax_addr, nmax, load_return);
		addedInsts.push_back(nmax_set_call);

		// Set value for gmp min
    	AllocaInst *min_addr = new AllocaInst (mpq, "", load_return);
    	addedInsts.push_back(min_addr);
		CallInst *min_init_call = mpq_init_call(min_addr,load_return);
		addedInsts.push_back(min_init_call);

		CallInst *min_set_call = mpq_set_d_call(min_addr, min, load_return);
		addedInsts.push_back(min_set_call);

		// Set value for gmp -min
    	AllocaInst *nmin_addr = new AllocaInst (mpq, "", load_return);
    	addedInsts.push_back(nmin_addr);
		CallInst *nmin_init_call = mpq_init_call(nmin_addr,load_return);
		addedInsts.push_back(nmin_init_call);

		CallInst *nmin_set_call = mpq_set_d_call(nmin_addr, nmin, load_return);
		addedInsts.push_back(nmin_set_call);

		// Set value for gmp zero
    	AllocaInst *zero_addr = new AllocaInst (mpq, "", load_return);
		addedInsts.push_back(zero_addr);

		CallInst *zero_init_call = mpq_init_call(zero_addr,load_return);
		addedInsts.push_back(zero_init_call);

		CallInst *zero_set_call = mpq_set_d_call(zero_addr, zero_fp, load_return);
    	addedInsts.push_back(zero_set_call);

    	// Compare operations
    	CallInst *max_cmp_call = mpq_cmp_call(val_addr, max_addr, load_return);
    	addedInsts.push_back(max_cmp_call);
    	CallInst *nmax_cmp_call = mpq_cmp_call(val_addr, nmax_addr, load_return);
    	addedInsts.push_back(nmax_cmp_call);
    	CallInst *min_cmp_call = mpq_cmp_call(val_addr, min_addr, load_return);
    	addedInsts.push_back(min_cmp_call);
    	CallInst *nmin_cmp_call = mpq_cmp_call(val_addr, nmin_addr, load_return);
    	addedInsts.push_back(nmin_cmp_call);
    	CallInst *zero_cmp_call = mpq_cmp_call(val_addr, zero_addr, load_return);
		addedInsts.push_back(zero_cmp_call);

		CmpInst *max_cmpOp = CmpInst::Create(
			Instruction::ICmp, ICmpInst::ICMP_SLT,
			zero_32, max_cmp_call, "", load_return
		);
		addedInsts.push_back(max_cmpOp);
		CmpInst *nmax_cmpOp = CmpInst::Create(
			Instruction::ICmp, ICmpInst::ICMP_SLT,
			nmax_cmp_call, zero_32, "", load_return
		);
		addedInsts.push_back(nmax_cmpOp);
		CmpInst *min_cmpOp = CmpInst::Create(
			Instruction::ICmp, ICmpInst::ICMP_SLT,
			min_cmp_call, zero_32, "", load_return
		);
		addedInsts.push_back(min_cmpOp);

		CmpInst *nmin_cmpOp = CmpInst::Create(
			Instruction::ICmp, ICmpInst::ICMP_SLT,
			zero_32, nmin_cmp_call, "", load_return
		);
		addedInsts.push_back(nmin_cmpOp);
		CmpInst *zero_cmpOp = CmpInst::Create(
		Instruction::ICmp, ICmpInst::ICMP_NE,
    	    					zero_32, zero_cmp_call, "", load_return);
		addedInsts.push_back(zero_cmpOp);

		BinaryOperator *andCond = BinaryOperator::Create(
				Instruction::And,
				min_cmpOp, nmin_cmpOp, "", load_return
		);
		addedInsts.push_back(andCond);

		andCond = BinaryOperator::Create(
				Instruction::And, andCond, zero_cmpOp,
				"", load_return
		);
		addedInsts.push_back(andCond);

		BinaryOperator *orCond = BinaryOperator::Create(
				Instruction::Or,
				max_cmpOp, nmax_cmpOp, "", load_return
		);
		addedInsts.push_back(orCond);

		orCond = BinaryOperator::Create(
				Instruction::Or, orCond, andCond, "",
				load_return
		);
		addedInsts.push_back(orCond);

		BasicBlock *preReturnBB = load_return->getParent();
		Function *mainFn = preReturnBB->getParent();
		BasicBlock *storeRetBB = BasicBlock::Create(
				value->getContext(), "storeRet",
				mainFn
		);
		StoreInst *storeInst = new StoreInst(
				one_32, ret_addr, false, 8,
				storeRetBB
		);
		addedInsts.push_back(storeInst);

		BasicBlock *returnBB = BasicBlock::Create(
				value->getContext(), "lastBB",
				mainFn
		);

		BranchInst::Create(returnBB, storeRetBB);
		//addedInsts.push_back(returnBr);

		preReturnBB->moveBefore(storeRetBB);
		storeRetBB->moveBefore(returnBB);
		Instruction *returnInst = preReturnBB->getTerminator();
		BranchInst *brInst = BranchInst::Create(
				returnBB, storeRetBB, orCond,
				preReturnBB
		);
		addedInsts.push_back(brInst);

		load_return->removeFromParent();
		returnInst->removeFromParent();
		returnBB->getInstList().push_back(load_return);
		printIntValue(load_return, returnBB);
		returnBB->getInstList().push_back(returnInst);

		WriteBitcodeToFile(&M, *out);

		//Clean instructions in main
		while (!addedInsts.empty()) {
			Instruction *I = addedInsts.back();
			I->eraseFromParent();
			addedInsts.pop_back();
		}
		load_return->removeFromParent();
		returnInst->removeFromParent();
		preReturnBB->getInstList().push_back(load_return);
		preReturnBB->getInstList().push_back(returnInst);
		storeRetBB->eraseFromParent();
		returnBB->eraseFromParent();
	}
	
	void processAllocate(Instruction *I, Instruction *nextInst) {
		Value *op = I->getUnderlyingObject();
		//Handle allocating a struct
		if (
              	const PointerType *ptrType = dyn_cast<PointerType> (
              		op->getType()
              	)
        ) {
			if (
      				const StructType *STy = dyn_cast<StructType> (
              			ptrType->getElementType()
              		)
           	) {
				unsigned numOfElements = STy->getNumElements();
				for (unsigned i = 0; i < numOfElements; i++) {
					const Type* elemType = STy->getElementType(i);
					if (elemType == mpq) {
						Value *idx[] = { ConstantInt::get(Type::getInt32Ty(
								I->getContext()), 0), ConstantInt::get(
								Type::getInt32Ty(I->getContext()), i) };
						GetElementPtrInst *element =
								GetElementPtrInst::CreateInBounds(op, idx, idx
										+ 2, "", nextInst);
						mpq_init_call(element, nextInst);
					}
				}
			}
		}
	}
	
    // Add function call, evaluate the value. Compare with max.
    // If it is real exceptional case return 1, otherwise return 0
    void addFunctionCalls (
    	Module &M, Instruction *ret_addr,
    	Instruction *load_return, Value *argv
    ) {
    	//Add function call
    	for (Module::iterator FF = M.begin(), FE = M.end(); FF != FE; FF++) {
    		Function *function = FF;
    		//Ignore some functions
    		if (
    			function->isDeclaration() ||
    			function->getName().startswith("setConstants") ||
    			function->getName().startswith("setGlobals") ||
    			function->getName().startswith("main")
    		)
    			continue;

    		std::string outputFileName =
    				"main_" + function->getName().str() + ".o";
    		std::string ErrInfo;
    		raw_ostream *out = new raw_fd_ostream(
    			outputFileName.c_str(), ErrInfo, raw_fd_ostream::F_Binary
    		);
    		const FunctionType *fnType = function->getFunctionType();
    		unsigned paramNum = fnType->getNumParams();
    		std::vector < Value *> params;
    		std::vector < Instruction *> addedInsts;
    		int numOfMpqVars = 0;
    		for (unsigned i = 0; i < paramNum; i++) {
    			const Type* type = fnType->getParamType(i);
    			if ( type == mpq ) {
    				AllocaInst *mpq_alloc = new AllocaInst(mpq, "", load_return);
    				Instruction *init_call = mpq_init_call(mpq_alloc, load_return);
    				addedInsts.push_back(mpq_alloc);
    				addedInsts.push_back(init_call);
    				Constant *index = ConstantInt::get(
    						int32, numOfMpqVars+1
    				);
    				Instruction *elem = GetElementPtrInst::CreateInBounds(
    						argv, index, "", load_return
    				);
    				addedInsts.push_back(elem);
    				LoadInst *load_elem = new LoadInst(
    						elem, "", load_return
    				);
    				load_elem->setAlignment(8);
    				addedInsts.push_back(load_elem);
    				Instruction *set_str_call = mpq_set_str_call(
    						mpq_alloc, load_elem,
    						ConstantInt::get(
    								IntegerType::getInt32Ty(M.getContext()), 10
    						),
    						load_return
    				);
    				addedInsts.push_back(set_str_call);
    				LoadInst *mpq_load = new LoadInst(
    						mpq_alloc, "", load_return
    				);
    				mpq_load->setAlignment(8);
    				addedInsts.push_back(mpq_load);
    				params.push_back(mpq_load);
    				numOfMpqVars++;
    			}

    			//FIXME: handle other types like struct, array and pointer
    			else if (type == mpq_Ptr) {
    				AllocaInst *alloc = new AllocaInst (
    					mpq, "", load_return
    				);
    				mpq_init_call(alloc, load_return);
    				params.push_back(alloc);
    			}
    			else if (const PointerType* ptrTy = dyn_cast<PointerType> (type)) {
    				AllocaInst *alloc = new AllocaInst (
    					ptrTy->getElementType(), "", load_return
    				);
    				processAllocate(alloc, load_return);
    				params.push_back(alloc);
    			}
    			else {
    				AllocaInst *alloc = new AllocaInst (
    					type, "", load_return
    				);
    				LoadInst *load = new LoadInst (alloc, "", load_return);
    				load->setAlignment(8);
    				params.push_back(load);
    			}
    		}

    		CallInst *function_call = CallInst::Create (
    				function, params.begin(), params.end(),
    				"", load_return
    		);
    		addedInsts.push_back(function_call);

    		if (fnType->getReturnType() == mpq) {
    			/*
    			 *  Set return value as default -1, change to 1 if the result is
    			 *  interesting one
    			 */
    			checkValue(
    				function_call, addedInsts, ret_addr, load_return,
    				M, out
    			);
    			continue;
    		}
    		/// FIXME: Check the values in gsl_result parameters
    		else {
    			bool exit = false;
    			for (unsigned i = 0; i < paramNum; i++) {
    				const Type* type = fnType->getParamType(i);
    				Value* param = params.at(i);
    				if (type == mpq_gsl_result_ptr) {
    					Value *idx[] = {
    						ConstantInt::get(
    							Type::getInt32Ty(M.getContext()), 0
    						),
    						ConstantInt::get(
    							Type::getInt32Ty(M.getContext()), 0
    						)
    					};
    					GetElementPtrInst *element =
    							GetElementPtrInst::CreateInBounds (
    									param, idx, idx+2, "", load_return
    							);
    					addedInsts.push_back(element);

    					LoadInst* val = new LoadInst(element, "", load_return);
    					addedInsts.push_back(val);

    					checkValue (
    						val, addedInsts, ret_addr, load_return, M, out
    					);
    					exit = true;
    					break;
    				}
    			}

    			if (exit)
    				continue;

    			WriteBitcodeToFile(&M, *out);

				//Clean instructions in main
				while (!addedInsts.empty()) {
					Instruction *I = addedInsts.back();
					I->eraseFromParent();
					addedInsts.pop_back();
				}
    		}
    	}
    }

    void init(Module &M) {
    	/*
    	 * Add new main function if necessary
    	 */
    	int32 = Type::getInt32Ty(M.getContext());
    	// Get type of argv: i8**
    	argvType = PointerType::getUnqual(Type::getInt8PtrTy(M.getContext()));
    	mainFn = cast<Function> (M.getOrInsertFunction(
    			"main", int32, int32, argvType,	NULL)
    	);
    	mpq = dyn_cast<StructType> (M.getTypeByName("struct.__mpq_struct"));
    	mpqArray = ArrayType::get(mpq, 1);
    	mpq_Ptr = PointerType::getUnqual(mpq);
    	mpqArray_Ptr = PointerType::getUnqual(mpqArray);
    	mpq_init = cast<Function>(
    		M.getOrInsertFunction(
    			"__gmpq_init", Type::getVoidTy(M.getContext()),
    			mpq_Ptr, NULL
    		)
    	);

    	mpq_clear = cast<Function>(
    			M.getOrInsertFunction("__gmpq_clear", Type::getVoidTy(M.getContext()),
    			mpq_Ptr, NULL)
    	);

    	mpq_set_d = cast<Function>(
    			M.getOrInsertFunction("__gmpq_set_d", Type::getVoidTy(M.getContext()),
    			mpq_Ptr, Type::getDoubleTy(M.getContext()), NULL)
    	);

    	mpq_set = cast<Function>(
    			M.getOrInsertFunction("__gmpq_set", Type::getVoidTy(M.getContext()),
    			mpq_Ptr, mpq_Ptr, NULL)
    	);

    	mpq_get_d = cast<Function>(
    			M.getOrInsertFunction(
    					"__gmpq_get_d", Type::getDoubleTy(M.getContext()
    			),
    	        PointerType::getUnqual(mpq), NULL)
    	);
    	mpq_set_str = cast<Function>( M.getOrInsertFunction(
    			"__gmpq_set_str", int32, mpq_Ptr,
    			IntegerType::getInt8PtrTy(M.getContext()),
    			int32, NULL)
    	);

    	mpq_cmp = cast<Function>(
    			M.getOrInsertFunction(
    					"__gmpq_cmp", Type::getInt32Ty(M.getContext()
    			),
    			PointerType::getUnqual(mpq), PointerType::getUnqual(mpq), NULL)
    	);

    	Constant *noArgument_msg = ConstantArray::get(
    			M.getContext(), "No arguments!\n", true
    	);
    	no_Argument = new GlobalVariable(
    			M,
    			noArgument_msg->getType(),
    			true,
    			GlobalValue::InternalLinkage,
    			noArgument_msg,
    			"no_Argument"
    	);

    	Constant *float_msg = ConstantArray::get(
    			M.getContext(), "%e\n", true
    	);
    	floatMsg = new GlobalVariable(
    			M,
    			float_msg->getType(),
    			true,
    			GlobalValue::InternalLinkage,
    			float_msg,
    			"floatValue"
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

    	//Declare printf function
    	std::vector<const Type*> ft_printf_args;
    	ft_printf_args.push_back(
    		PointerType::getUnqual(IntegerType::getInt8Ty(M.getContext()))
    	);

    	FunctionType* ft_printf = FunctionType::get(
    			int32, ft_printf_args, true
    	);

    	printf = cast<Function>(
    			M.getOrInsertFunction("printf", ft_printf)
    	);

    	zero_32 = ConstantInt::get(	int32, 0);
    	one_32 = ConstantInt::get( int32, 1);
    	mone_32 = ConstantInt::get( int32, -1);

    	std::vector<const Type *> typeParams;
    	typeParams.push_back(mpq);
    	typeParams.push_back(mpq);
    	mpq_gsl_result = StructType::get(
    		M.getContext(),typeParams
    	);
    	mpq_gsl_result_ptr = PointerType::getUnqual(mpq_gsl_result);
    }

    // Debugging
    void printErrMsg(
    		GlobalVariable *msg, BasicBlock *bb
    ) {

    	Constant *gep_params[] = {
    			zero_32,
    			zero_32
    	};

    	Constant *errMsg = ConstantExpr::getGetElementPtr(
    			msg, gep_params,
    			array_lengthof(gep_params)
    	);

    	Value *params[] = {
    			errMsg
    	};

    	CallInst *print_call = CallInst::Create(printf,
    			params, array_endof(params),
    			"", bb
    	);
    	print_call->setTailCall(true);
    }

    CallInst* printFloatValue (
    		Value *val, Instruction *inst
    ) {

    	Constant *gep_params[] = {
    			zero_32,
    			zero_32
    	};

    	Constant *fmsg = ConstantExpr::getGetElementPtr(
    			floatMsg, gep_params,
    			array_lengthof(gep_params)
    	);

    	Value *params[] = {
    			fmsg,
    			val
    	};

    	CallInst *print_call = CallInst::Create(
    			printf,	params, array_endof(params),
    			"", inst
    	);
    	print_call->setTailCall(true);
    	return print_call;
    }

    void printFloatValue(Value *val, BasicBlock *bb) {

		Constant *gep_params[] = { zero_32, zero_32 };

		Constant *fmsg = ConstantExpr::getGetElementPtr(
				floatMsg, gep_params,
				array_lengthof(gep_params)
		);

		Value *params[] = { fmsg, val };
		CallInst *print_call = CallInst::Create(
				printf, params, array_endof(
				params), "", bb
		);
		print_call->setTailCall(true);
	}


    void printIntValue (
    		Value *val, BasicBlock *bb
    ) {
       	Constant *gep_params[] = {
   			zero_32,
   			zero_32
      	};

      	Constant *msg = ConstantExpr::getGetElementPtr(
   			intMsg, gep_params,
   			array_lengthof(gep_params)
      	);

      	Value *params[] = {
   			msg,
   			val
      	};

      	CallInst *print_call = CallInst::Create(
   			printf,	params, array_endof(params),
   			"", bb
      	);
       	print_call->setTailCall(true);
    }

    void printIntValue (
    		Value *val, Instruction *inst
    ) {

        Constant *gep_params[] = {
        	zero_32,
        	zero_32
        };

        Constant *msg = ConstantExpr::getGetElementPtr(
         	intMsg, gep_params,
         	array_lengthof(gep_params)
        );

        Value *params[] = {
        	msg,
        	val
        };

        CallInst *print_call = CallInst::Create(
        	printf,	params, array_endof(params),
        	"", inst
        );
       	print_call->setTailCall(true);
   }

    virtual bool runOnModule(Module &M) {
    	init(M);
    	Function::ArgumentListType::iterator firstArg =
    			mainFn->getArgumentList().begin();
    	Value *argc = firstArg;
    	argc->setName("argc");
    	Value *argv = ++firstArg;
    	argv->setName("argv");

    	//Construct main entry basic block
    	BasicBlock *mainEntry = BasicBlock::Create(
    			M.getContext(), "entry", mainFn
    	);

    	//FIXME: May not need to set length 8
    	Instruction *ret_addr = new AllocaInst (int32, "ret", mainEntry);
    	new StoreInst (zero_32, ret_addr, false, 8, mainEntry);

    	BasicBlock *returnBB = BasicBlock::Create(
    			M.getContext(), "return", mainFn
    	);

    	LoadInst *load_return = new LoadInst (ret_addr, "", returnBB);
    	load_return->setAlignment(8);
    	ReturnInst::Create (
    		M.getContext(), load_return , returnBB
    	);

    	BranchInst::Create (
    		returnBB, mainEntry
    	);

    	//Add setConstants functions
    	Instruction *firstInst = mainFn->getEntryBlock().getFirstNonPHI();
    	for (
    		Module::global_iterator G = M.global_begin(),
    		GE = M.global_end(); G != GE; ++G
    	) {
    		GlobalVariable *glVar = G;
    		//ignore __ariadne variables
    		if (glVar->getName().startswith("__ariadne") ||
    			glVar->getName().startswith("no_") ||
    			glVar->getName().startswith("floatValue") ||
    			glVar->getName().startswith("intValue") ||
    			glVar->getName().startswith("main") ||
    			glVar->getName().startswith("llvm.global_ctors"))
    			continue;
    		//ignore integer type global variables
    		const PointerType *ptrType = dyn_cast<PointerType> (glVar->getType());
    		if (ptrType->getElementType()->isIntOrIntVectorTy())
    			continue;
    		if (glVar->isDeclaration())
    			continue;
    		//ignore string globals
    		if ( const ArrayType *ATy = dyn_cast <ArrayType>(
    				ptrType->getElementType())
    		) {
    			if (ATy->getElementType() == IntegerType::getInt8Ty(G->getContext())) {
    				continue;
    			}
    		}
    		std::string gName = glVar->getName().str();
    		std::string setConstantStr = "setConstants_" + gName;
    		setConstants = cast<Function>(M.getOrInsertFunction(
    				setConstantStr, Type::getVoidTy(M.getContext()), NULL)
    		);
			CallInst::Create (setConstants, "", firstInst);
    	}
    	// Add function calls
    	addFunctionCalls(M, ret_addr, load_return, argv);

    	//remove main from the module
    	mainFn->eraseFromParent();
    	M.print(errs(),&w); // Debugging
    	return true;
    }
  };
}

char APMainAdder::ID = 0;
static RegisterPass<APMainAdder> X("APMainAdder", "Arbitrary Precision Main Adder Pass");
