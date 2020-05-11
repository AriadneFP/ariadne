/* This transformation will change all types in input programs to gmp_q types
* The transformation is to distinguish the uninteresting / interesting
* exceptions. The transformed code will be executed with gmp library to compare
* the result of original exception cases with the execution in infinitive real
* number semantics
*/

#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/Type.h"
#include "llvm/GlobalVariable.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Assembly/AsmAnnotationWriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Instructions.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/AbstractTypeUser.h"
#include "llvm/Constants.h"

using namespace llvm;

namespace {
  struct ArbitraryPrecision : public ModulePass {
    static char ID; // Pass identification, replacement for typeid

    AssemblyAnnotationWriter w; //debugging

    const StructType *mpq; //mpq_t type
    const Type *mpq_Ptr; // mpq_t pointer type
    const ArrayType *mpqArray; // mpq_array type

	/* gmp standard functions*/
    Function *mpq_init;
    Function *mpq_set_d;
    Function *mpq_add;
    Function *mpq_sub;
    Function *mpq_mul;
    Function *mpq_div;
    Function *mpq_clear;
    Function *mpq_get_d;
    Function *mpq_cmp;
    Function *mainFn;
    Function *setConstants;
    Function *setGlobals; // Initialize all globals in a module
    //Function *mpq_inp_str;
    BasicBlock *setConstantsBB;
    BasicBlock *setGlobalsBB;
    Instruction *setGlobalsTeminator;

    /* Types of operations*/
    enum OpId {
    	//Arithmetics
    	Add,
    	Sub,
    	Mul,
    	Div,
    	//Comparisons
    	Gt,
    	Ge,
    	Lt,
    	Le,
    	Eq,
    	Ne
    };

    ArbitraryPrecision() : ModulePass(&ID) {}

    virtual bool runOnModule( Module &M){
    	doInitialization (M);
    	for (
    			Module::global_iterator G = M.global_begin(),
    			GE = M.global_end(); G != GE; ++G
    	){
    		GlobalVariable *glVar = G;
    		runOnGlobal (glVar);
    	}

    	for (Module::iterator F = M.begin(), ME = M.end(); F != ME; ++F){
    		Function *function = F;    		
    		runOnFunction(*function);
    	}
    	doFinalization(M);
    	return true;
    }

    /*
     * FIXME: Should have a general function getNewType for a general type,
     * not specific to StructType
     */

    // Construct a new struct type
    StructType* getNewType(const StructType* STy){
    	std::vector<const Type *> typeParams;
    	for (
    			StructType::element_iterator EB = STy->element_begin(),
											 EI = EB,
											 EE = STy->element_end();
    			EI != EE; ++EI
    	) {

    		const Type *ty = EI->get();
    		// FIXME: The case pointer to a struct type

    		if (const StructType *sty = dyn_cast<StructType> (ty) )
    			typeParams.push_back(getNewType(sty));
    		else if (const ArrayType *aty = dyn_cast<ArrayType> (ty)){
    			if (
    				aty->getElementType() == Type::getDoubleTy(STy->getContext()) ||
    				aty->getElementType() == Type::getFloatTy(STy->getContext())
    			) {
    				ArrayType *newArrayType = ArrayType::get(mpq,aty->getNumElements());
    				typeParams.push_back(newArrayType);
    			}else {
    				typeParams.push_back(ty);
    			}
    		}
    		else if (ty == Type::getDoubleTy(STy->getContext()) ||
    				ty == Type::getFloatTy(STy->getContext()))
    			typeParams.push_back(mpq);
    		else if (ty == Type::getDoublePtrTy(STy->getContext()) ||
    		    					ty == Type::getFloatPtrTy(STy->getContext()))
    		    				typeParams.push_back(mpq_Ptr);
    		else if (const PointerType *ptrTy = dyn_cast<PointerType>(ty)){
    			if (
    					const StructType *sty = dyn_cast<StructType>(
    							ptrTy->getElementType()
    					)
    			) {
    				StructType *newSty = getNewType(sty);
    				typeParams.push_back(PointerType::getUnqual(newSty));
    			}
    			else
    				typeParams.push_back(ty);
    			//FIXME: handle the array type
    		}
    		else
    			typeParams.push_back(ty);
    	}
    	StructType *newStructType = StructType::get(
    			STy->getContext(),typeParams
    	);

    	return newStructType;
    }

    // Get original struct type
    StructType* getOriginalType(const StructType* STy){

    	std::vector<const Type *> typeParams;
    	for (
    			StructType::element_iterator EB = STy->element_begin(),
    			EI = EB,
    			EE = STy->element_end();
    			EI != EE; ++EI
    	) {

    		const Type *ty = EI->get();
    		if (ty == mpq)
    			typeParams.push_back(Type::getDoubleTy(STy->getContext()));
    		else if (ty == mpq_Ptr )
    			typeParams.push_back(Type::getDoublePtrTy(STy->getContext()));
    		// FIXME: The case pointer to a struct type
    		else if (const StructType *sty = dyn_cast<StructType> (ty) )
    			typeParams.push_back(getNewType(sty));
    		else if (const ArrayType *aty = dyn_cast<ArrayType> (ty)){
    			if ( aty->getElementType() ==  mpq ) {
        				ArrayType *newArrayType = ArrayType::get (
        						Type::getDoubleTy(STy->getContext()),
        						aty->getNumElements()
        				);
        				typeParams.push_back(newArrayType);
        		} else {
        			typeParams.push_back(ty);
        		}
    		}
    		else if (const PointerType *ptrTy = dyn_cast<PointerType>(ty)){
    			if (
    					const StructType *sty = dyn_cast<StructType>(
    							ptrTy->getElementType()
    					)
    			) {
    				StructType *newSty = getNewType(sty);
    				typeParams.push_back(PointerType::getUnqual(newSty));
    			}
    			else
    				typeParams.push_back(ty);
					//FIXME: handle the array type
    		} else
        			typeParams.push_back(ty);
      	}
       	StructType *originalStructType = StructType::get(
       			STy->getContext(),typeParams
       	);
       	return originalStructType;
    }

    // Traverse all struct type and recursively change types in the struct
    void handleStructGlobal (
    	GlobalVariable *G, const StructType *STy,bool needToSetConstants = true
    ) {

    	if (STy == mpq)
    		return;

    	const Type *newSTy = getNewType(STy);

    	AbstractTypeUser::setType(
    			G, PointerType::getUnqual(newSTy)
    	);
    	if (!G->hasInitializer())
    		return;
    	Value *element = G->getInitializer();
    	AbstractTypeUser::setType(
    			element, newSTy
    	);

    	if (
    		ConstantStruct *structConst = dyn_cast<ConstantStruct> (
    				element
    		)
    	) {
    		unsigned numOfOps = structConst->getNumOperands();

    		for (unsigned i = 0; i < numOfOps; i++){
    			Value *operand = structConst->getOperand(i);
    			if (ConstantFP* C = dyn_cast<ConstantFP>(
    					structConst->getOperand(i))
    			) {
    				if (needToSetConstants) {
    					Value *idx[] = {
    						ConstantInt::get(
    								Type::getInt32Ty(G->getContext()), 0
    						),
    						ConstantInt::get(
    								Type::getInt32Ty(G->getContext()), i
    						)
    					};
    					GetElementPtrInst *element = GetElementPtrInst::CreateInBounds (
    						G, idx, idx+2, "", setConstantsBB
    					);
    					mpq_init_call(element,setConstantsBB);
    					mpq_set_d_call(element, C, setConstantsBB);
    					AbstractTypeUser::setType(element, mpq_Ptr);
    					structConst->setOperand(i,Constant::getNullValue(mpq));
    				} else {
    					AbstractTypeUser::setType(operand, mpq);
    				}
    			}
    			else if (operand->getType() == Type::getDoubleTy(G->getContext()))
    				AbstractTypeUser::setType(operand, mpq);
    			else if (operand->getType() == Type::getDoublePtrTy(G->getContext()))
    				AbstractTypeUser::setType(operand, mpq_Ptr);
    		}
    	}
    }

    // Change floating point variables to gmp types
    // Set all constants in setConstants basic block
    void handleFloatingGlobal(GlobalVariable *G, bool needToSetConstants = true) {
    	Value *val = dyn_cast<Value> (G);
    	AbstractTypeUser::setType(val, mpq_Ptr);
    	if (needToSetConstants) {
    		ConstantFP *C = dyn_cast<ConstantFP> (G->getInitializer());
    		mpq_init_call(G,setConstantsBB);
    		mpq_set_d_call(G, C, setConstantsBB);
    	}
    	Constant *initializer = Constant::getNullValue(mpq);
    	G->setInitializer(initializer);
    }

    void handleFloatingArray (
    		GlobalVariable *G, ConstantArray *arrayConst,
    		bool needToSetConstants = true
    ) {
    	std::vector <Constant> constList;
    	unsigned numOfElements = arrayConst->getNumOperands();
    	ArrayType *newArrayType = ArrayType::get(mpq,numOfElements);
    	AbstractTypeUser::setType(G, PointerType::getUnqual(newArrayType));
    	for (unsigned i=0; i< numOfElements; i++){
    		if (needToSetConstants) {
    			ConstantFP* val = dyn_cast<ConstantFP>(
    				arrayConst->getOperand(i)
    			);
    			Value *idx[] = {
    				ConstantInt::get(
    						Type::getInt32Ty(G->getContext()), 0
    				),
    				ConstantInt::get(
    						Type::getInt32Ty(G->getContext()), i
    				)
    			};
    			GetElementPtrInst *element = GetElementPtrInst::CreateInBounds (
    				G, idx, idx+2, "", setConstantsBB
    			);
    			mpq_init_call(element,setConstantsBB);
    			mpq_set_d_call(element, val, setConstantsBB);
    		}
    	}
    	Constant *initializer = Constant::getNullValue(newArrayType);
    	G->setInitializer(initializer);
    	AbstractTypeUser::setType(arrayConst, newArrayType);
    }

    void handleStructArray(
    		GlobalVariable *G, const ArrayType *ATy,
    		const StructType *elemSTy, ConstantArray *arrayConst,
    		bool needToSetConstants = true
    ) {
    	// Ignore __ariadne_deleme added by ariadne.rb
    	if (elemSTy == mpq)
    		return;

    	const Type *newElemSTy = getNewType(elemSTy);
    	ArrayType *newArrayType = ArrayType::get(
    			newElemSTy,
    			ATy->getNumElements()
    	);

    	unsigned numOfElements = arrayConst->getNumOperands();

    	AbstractTypeUser::setType(
    			G, PointerType::getUnqual(newArrayType)
    	);
    	AbstractTypeUser::setType(
    			G->getInitializer(), newArrayType
    	);

    	for (unsigned i = 0; i < numOfElements; i++) {
    		Value *elem = arrayConst->getOperand(i);
    		if (
    			ConstantStruct *structConst = dyn_cast<ConstantStruct> (
    					elem
    			)
    		) {
    			StructType* newStructType = getNewType(structConst->getType());
    			unsigned numOfOps = structConst->getNumOperands();
    			AbstractTypeUser::setType(
    				elem, newStructType
    			);
    			for (unsigned j = 0; j < numOfOps; j++){
    				Value *operand = structConst->getOperand(j);
    				if (ConstantFP* C = dyn_cast<ConstantFP>(operand)) {
    					if (needToSetConstants) {
    						structConst->setOperand(
								j, Constant::getNullValue(mpq)
							);
    						Value *idx[] = {
    							ConstantInt::get(
    									Type::getInt32Ty(G->getContext()), 0
    							),
    							ConstantInt::get(
    									Type::getInt32Ty(G->getContext()), i
    							),
    							ConstantInt::get(
    									Type::getInt32Ty(G->getContext()), j
    							)
    						};
 			    			GetElementPtrInst *element = GetElementPtrInst::CreateInBounds(
 			    					G, idx, idx + 3, "", setConstantsBB
 			    			);
 			    			mpq_init_call(element, setConstantsBB);
 			    			mpq_set_d_call(element, C, setConstantsBB);
    					} else {
    						AbstractTypeUser::setType(operand, mpq);
    					}
    				}
    				else if (operand->getType() == Type::getDoubleTy(G->getContext()))
    					AbstractTypeUser::setType(operand, mpq);
    				else if (operand->getType() == Type::getDoublePtrTy(G->getContext()))
    					AbstractTypeUser::setType(operand, mpq_Ptr);
    			}
    		}
    	}
    }

    // Handle array of pointers
    void handlePointerArray (
    	GlobalVariable *G, const ArrayType *ATy,
    	const PointerType *elementPtrType, ConstantArray *arrayConst,
    	bool needToSetConstants = true
    ) {
    	if (
    			const ArrayType *elemATy = dyn_cast <ArrayType>(
    					elementPtrType->getElementType()
    			)
    	) {
    		if (
    				elemATy->getElementType() == Type::getDoubleTy(G->getContext()) ||
    				elemATy->getElementType() == Type::getFloatTy(G->getContext())
    		) {
    			//FIXME: need to change the type of each element too
    			ArrayType *newElemArrayType = ArrayType::get(
    					mpq, elemATy->getNumElements()
    			);
    			Type *newElemArrayPtrType = PointerType::getUnqual(
    					newElemArrayType
    			);

    			ArrayType *newArrayType = ArrayType::get(
    					newElemArrayPtrType, ATy->getNumElements()
    			);
    			AbstractTypeUser::setType(G, PointerType::getUnqual(newArrayType));
    			AbstractTypeUser::setType(arrayConst, newArrayType);
    		}
    	}
    	else if (
    			ATy->getElementType() == Type::getDoublePtrTy(G->getContext()) ||
    			ATy->getElementType() == Type::getFloatPtrTy(G->getContext())
    	) {

    		ArrayType *newArrayType = ArrayType::get(
    				mpq_Ptr,
    				ATy->getNumElements()
    		);

    		AbstractTypeUser::setType(G, PointerType::getUnqual(newArrayType));
    		AbstractTypeUser::setType(
    				G->getInitializer(), newArrayType
    		);

    		//Consider the constant elements
    		unsigned numOfElements = arrayConst->getNumOperands();
    		for (unsigned i = 0; i < numOfElements; i++) {
    			Value *element = arrayConst->getOperand(i);

    			if (
    				ConstantPointerNull::classof(element)
    			) {
    				arrayConst->setOperand(
    					i, Constant::getNullValue(mpq_Ptr)
    				);
    			}

    		}
    	}
    }

    // Handle array of constants
    void handleArrayGlobal(
    		GlobalVariable *G, const ArrayType *ATy, bool needToSetConstants = true
    ) {
    	if (!G->hasInitializer()) {
    		return;
    	}
    	Value *initializer = G->getInitializer();
    	if (ConstantArray *arrayConst = dyn_cast<ConstantArray> (initializer)) {
    		if (
    			ATy->getElementType() == Type::getDoubleTy(G->getContext()) ||
    			ATy->getElementType() == Type::getFloatTy(G->getContext())
    		) {
    			handleFloatingArray(G,arrayConst,needToSetConstants);
    		}
    		// Handle array of Pointers
    		else if (
    			const PointerType *elementPtrType = dyn_cast<PointerType> (
    					ATy->getElementType()
    			)
    		) {
    			handlePointerArray(G,ATy,elementPtrType, arrayConst, needToSetConstants);
    		}
    		// Handle arrays of Struct
    		else if (
    			const StructType *elemSTy = dyn_cast <StructType>(
    					ATy->getElementType()
    			)
    		) {
    			handleStructArray(G, ATy, elemSTy, arrayConst,needToSetConstants);
    		}
    	}
    }


    /* Traverse all globals and change the types, initilizers to gmp_q types */
    void runOnGlobal ( GlobalVariable *G){
    	//ignore __ariadne variables
    	if (G->getName().startswith("__ariadne") ||
    		G->getName().startswith("llvm.global_ctors"))
    		return;

    	const PointerType *ptrType = dyn_cast<PointerType> (G->getType());

    	//ignore integer global variables
    	if (ptrType->getElementType()->isIntOrIntVectorTy()) {
    		errs() << G->getName() << "\n";
    		return;
    	}

    	//ignore string globals
    	if ( const ArrayType *ATy = dyn_cast <ArrayType>(
    	    			ptrType->getElementType())
    	) {
    		if (ATy->getElementType() == IntegerType::getInt8Ty(G->getContext())) {
    			return;
    		}
    	}

    	//With external globals, change the types only
    	bool needToSetConstants = true;
    	if (G->isDeclaration())
    		needToSetConstants = false;
    	Module *M = G->getParent();
    	if (needToSetConstants) {

    		std::string gName = G->getName().str();
    		std::string setConstantStr = "setConstants_" + gName;
    		setConstants = cast<Function>(M->getOrInsertFunction (
    			setConstantStr, Type::getVoidTy(G->getContext()), NULL)
    		);
    		setConstants->setLinkage(GlobalValue::LinkOnceAnyLinkage);

    		setConstantsBB = BasicBlock::Create(
    			G->getContext(), "", setConstants
    		);
    		CallInst::Create (setConstants, "", setGlobalsTeminator);
    	}
    	// HACK: always set all globals to non-constant so we can modify the data
    	// in the transformation later.
    	if (G->isConstant()) {
    		G->setConstant(false);
    	}

    	// Handle struct global
    	if (
    			const StructType *STy = dyn_cast <StructType>(
    	    		ptrType->getElementType()
    			)
    	) {
    		handleStructGlobal(G,STy,needToSetConstants);
    	}

    	// Handle floating point globals
    	else if (G->getType() == PointerType::getDoublePtrTy(G->getContext()) ||
    			G->getType() == PointerType::getFloatPtrTy(G->getContext())){
    		handleFloatingGlobal(G,needToSetConstants);
    	}

    	// Handle array type for globals
    	else if ( const ArrayType *ATy = dyn_cast <ArrayType>(
    			ptrType->getElementType())
    	) {
    		handleArrayGlobal(G,ATy,needToSetConstants);
    	}
    	if (needToSetConstants)
    		ReturnInst::Create(G->getContext(), setConstantsBB);

    }

    /* Build the gmp function calls with parameters and basic blocks*/
    CallInst* mpq_init_call (Value *param, BasicBlock* bb){
    	Value *init_params[] = {
    			param
    	};

    	CallInst *init_call = CallInst::Create(mpq_init,
    			init_params, array_endof(init_params),
    			"", bb);
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

    CallInst* mpq_add_call (Value *res, Value *op1, Value *op2, BasicBlock* bb){
    	Value *add_params[] = {
    			res,
    			op1,
    			op2
    	};

    	CallInst *add_call = CallInst::Create(
    			mpq_add, add_params, array_endof(add_params), "", bb
    	);

    	return add_call;
    }

    CallInst* mpq_add_call (Value *res, Value *op1, Value *op2, Instruction* I){
       	Value *add_params[] = {
    		res,
    		op1,
    		op2
      	};

      	CallInst *add_call = CallInst::Create(
      			mpq_add, add_params, array_endof(add_params), "", I
      	);

      	return add_call;
    }

    CallInst* mpq_sub_call (Value *res, Value *op1, Value *op2, BasicBlock* bb){
    	Value *sub_params[] = {
    			res,
    			op1,
    			op2
    	};

    	CallInst *sub_call = CallInst::Create(
    			mpq_sub, sub_params, array_endof(sub_params), "", bb
    	);

    	return sub_call;
    }

    CallInst* mpq_sub_call (Value *res, Value *op1, Value *op2, Instruction* I){
    	Value *sub_params[] = {
    			res,
    			op1,
    			op2
    	};

    	CallInst *sub_call = CallInst::Create(
    			mpq_sub, sub_params, array_endof(sub_params), "", I
    	);

    	return sub_call;
    }

    CallInst* mpq_mul_call (Value *res, Value *op1, Value *op2, BasicBlock* bb){
        Value *mul_params[] = {
        		res,
        		op1,
        		op2
        };

        CallInst *mul_call = CallInst::Create(
        		mpq_mul, mul_params, array_endof(mul_params), "", bb
        );

        return mul_call;
    }

    CallInst* mpq_mul_call (Value *res, Value *op1, Value *op2, Instruction* I){
         Value *mul_params[] = {
         		res,
           		op1,
           		op2
         };

         CallInst *mul_call = CallInst::Create(
          		mpq_mul, mul_params, array_endof(mul_params), "", I
         );

         return mul_call;
    }

    CallInst* mpq_div_call (Value *res, Value *op1, Value *op2, BasicBlock* bb){
    	Value *div_params[] = {
    			res,
    			op1,
    			op2
    	};

    	CallInst *div_call = CallInst::Create(
    			mpq_div, div_params, array_endof(div_params), "", bb
    	);

    	return div_call;
    }

    CallInst* mpq_div_call (Value *res, Value *op1, Value *op2, Instruction* I){
       	Value *div_params[] = {
       			res,
       			op1,
       			op2
       	};

      	CallInst *div_call = CallInst::Create(
       			mpq_div, div_params, array_endof(div_params), "", I
       	);

       	return div_call;
    }


    CallInst* mpq_clear_call (Value *param, BasicBlock* bb){
    	Value *clear_params[] = {
    			param
    	};

    	CallInst *clear_call = CallInst::Create(mpq_clear,
    			clear_params, array_endof(clear_params),
    			"", bb);

    	return clear_call;
    }

    CallInst* mpq_set_d_call (Value *mpq_var, Value *val, BasicBlock *bb){
    	Value *set_params[] = {
    		mpq_var,
    		val
    	};

    	CallInst *mpq_set_d_call = CallInst::Create(
    			mpq_set_d, set_params, array_endof(set_params),
    			"", bb
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

    CallInst* mpq_cmp_call (Value *op1, Value *op2, BasicBlock *bb){
        	Value *cmp_params[] = {
        			op1, op2
        	};

        	CallInst *mpq_cmp_call = CallInst::Create(
        			mpq_cmp, cmp_params, array_endof(cmp_params),"", bb
        	);
        	return mpq_cmp_call;
    }

    /*Traverse operations change the instructions to appropriate function calls*/
    void processOp (
    		Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock, OpId op
    ) {
    	Constant *zero_dbl = ConstantFP::get(
    			Type::getDoubleTy(I->getContext()),0
    	);
    	Value *operand1 = I->getOperand(0);
    	Value *operand2 = I->getOperand(1);

    	if (operand1->getType() == Type::getDoubleTy(I->getContext())){
    		AbstractTypeUser::setType (operand1, mpq);
    	}

    	if (operand2->getType() == Type::getDoubleTy(I->getContext())){
    		AbstractTypeUser::setType (operand2, mpq);
    	}

    	Value *res = I->getUnderlyingObject();

    	AllocaInst *alloc_res = new AllocaInst(mpq,"",preBlock);
    	mpq_init_call (alloc_res, preBlock);
    	AllocaInst *alloc_op1 = new AllocaInst(mpq,"",preBlock);
    	mpq_init_call (alloc_op1, preBlock);
    	AllocaInst *alloc_op2 = new AllocaInst(mpq,"",preBlock);
    	mpq_init_call (alloc_op2, preBlock);

    	new StoreInst(
    			operand1, alloc_op1->getUnderlyingObject(), preBlock
    	);

    	new StoreInst(
    			operand2, alloc_op2->getUnderlyingObject(), preBlock
    	);

    	CallInst *op_call;
    	switch (op){
    	case Add:
    		op_call = mpq_add_call(
    				alloc_res->getUnderlyingObject(),
    				alloc_op1->getUnderlyingObject(),
    				alloc_op2->getUnderlyingObject(),
    				preBlock
    		);
    		break;

    	case Sub:
    		op_call = mpq_sub_call(
    				alloc_res->getUnderlyingObject(),
    				alloc_op1->getUnderlyingObject(),
    				alloc_op2->getUnderlyingObject(),
    				preBlock
    		);
    		break;
    	case Mul:
    		op_call = mpq_mul_call(
    				alloc_res->getUnderlyingObject(),
    				alloc_op1->getUnderlyingObject(),
    				alloc_op2->getUnderlyingObject(),
    				preBlock
    		);
    		break;
    	case Div:
    		op_call = mpq_div_call(
    				alloc_res->getUnderlyingObject(),
    				alloc_op1->getUnderlyingObject(),
    				alloc_op2->getUnderlyingObject(),
    				preBlock
    		);
    		break;
    	//This case does not appear
    	default:
    		break;
    	}

    	AbstractTypeUser::setType(res,mpq);
    	LoadInst *load_res = new LoadInst(
    			alloc_res->getUnderlyingObject(),"",preBlock
    	);

    	I->removeFromParent();
    	//FIXME: should we need these ?
    	I->setOperand(0,zero_dbl);
    	I->setOperand(1,zero_dbl);
    	I->replaceAllUsesWith(load_res->getUnderlyingObject());

    	BranchInst *br = BranchInst::Create(postBlock);
    	preBlock->getInstList().push_back(br);
    }

    void processCmp (
    	Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock, OpId op
    ) {
    	//Case: operands are floating point types
    	replaceType(I, preBlock);
    	Value *operand1 = I->getOperand(0);
    	Value *operand2 = I->getOperand(1);

    	AllocaInst *alloc_op1 = new AllocaInst(mpq,"",preBlock);
    	mpq_init_call(alloc_op1,preBlock);
    	AllocaInst *alloc_op2 = new AllocaInst(mpq,"",preBlock);
    	mpq_init_call(alloc_op2,preBlock);

    	new StoreInst(
    			operand1, alloc_op1->getUnderlyingObject(), preBlock
    	);
    	new StoreInst(
    			operand2, alloc_op2->getUnderlyingObject(), preBlock
    	);
    	CallInst* op_Call = mpq_cmp_call(
    			alloc_op1->getUnderlyingObject(),
    			alloc_op2->getUnderlyingObject(), preBlock
    	);
    	Constant *zero_32 = Constant::getNullValue(
    			IntegerType::getInt32Ty(I->getContext())
    	);
    	CmpInst *sgtOp, *sltOp, *eqOp, *sgeOp, *sleOp, *neOp;
    	switch (op){
    	case Ge:
    		sgeOp = CmpInst::Create(
    				Instruction::ICmp, ICmpInst::ICMP_SGE,
    				op_Call->getUnderlyingObject(), zero_32 , "", preBlock
    		);
    		I->replaceAllUsesWith(sgeOp);
    		I->eraseFromParent();
    		break;

    	case Gt:
    		sgtOp = CmpInst::Create(
    				Instruction::ICmp, ICmpInst::ICMP_SGT,
    				op_Call->getUnderlyingObject(), zero_32 , "", preBlock
    		);
    		I->replaceAllUsesWith(sgtOp);
    		I->eraseFromParent();
    		break;

    	case Le:
    		sleOp = CmpInst::Create(
    				Instruction::ICmp, ICmpInst::ICMP_SLE,
    				op_Call->getUnderlyingObject(), zero_32 , "", preBlock
    		);
    		I->replaceAllUsesWith(sleOp);
    		I->eraseFromParent();
    		break;

    	case Lt:
    		sltOp = CmpInst::Create(
    				Instruction::ICmp, ICmpInst::ICMP_SLT,
    				op_Call->getUnderlyingObject(), zero_32 , "", preBlock
    		);
    		I->replaceAllUsesWith(sltOp);
    		I->eraseFromParent();
    		break;

    	case Eq:
    		eqOp = CmpInst::Create(
    				Instruction::ICmp, ICmpInst::ICMP_EQ,
    				op_Call->getUnderlyingObject(), zero_32 , "", preBlock
    		);
    		I->replaceAllUsesWith(eqOp);
    		I->eraseFromParent();
    		break;

    	case Ne:
    		neOp = CmpInst::Create(
    				Instruction::ICmp, ICmpInst::ICMP_NE,
    				op_Call->getUnderlyingObject(), zero_32 , "", preBlock
    		);
    		I->replaceAllUsesWith(neOp);
    		I->eraseFromParent();
    		break;
    	// This case does not appear
    	default:
    		break;
    	}
    	BranchInst *br = BranchInst::Create(postBlock);
    	preBlock->getInstList().push_back(br);
    }

    /* Traverse instructions, do the transformations on these instructions */
    void processInstruction(
    		Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock
    ) {

    	switch (I->getOpcode()){

			//Floating point operators
			//Addition
			case Instruction::FAdd: {
				processOp(I, preBlock,postBlock, Add);
				break;
			}

			//Subtraction
			case Instruction::FSub: {
				processOp(I, preBlock,postBlock, Sub);
				break;
			}

			//Multiplication
			case Instruction::FMul: {
				processOp(I, preBlock,postBlock, Mul);
				break;
			}

			//Division
			case Instruction::FDiv: {
				processOp(I, preBlock, postBlock, Div);
				break;
			}
			
			//Comparison operations
			case Instruction::FCmp:{
				FCmpInst *fi = cast<FCmpInst>(I);
				switch (fi->getPredicate()){
					case FCmpInst::FCMP_UEQ:
						processCmp(I, preBlock, postBlock, Eq);
						break;

					case FCmpInst::FCMP_OEQ:
						processCmp(I, preBlock, postBlock, Eq);
						break;

					case FCmpInst::FCMP_UNE:
						processCmp(I, preBlock, postBlock, Ne);
						break;
					case FCmpInst::FCMP_ONE:
						processCmp(I, preBlock, postBlock, Ne);
						break;
					case FCmpInst::FCMP_UGT:
						processCmp(I, preBlock, postBlock, Gt);
						break;

					case FCmpInst::FCMP_OGT:
						processCmp(I, preBlock, postBlock, Gt);
						break;

					case FCmpInst::FCMP_UGE:
						processCmp(I, preBlock, postBlock, Ge);
						break;

					case FCmpInst::FCMP_OGE:
						processCmp(I, preBlock, postBlock, Ge);
						break;

					case FCmpInst::FCMP_ULT:
						processCmp(I, preBlock, postBlock, Lt);
						break;

					case FCmpInst::FCMP_OLT:
						processCmp(I, preBlock, postBlock, Lt);
						break;

					case FCmpInst::FCMP_ULE:
						processCmp(I, preBlock, postBlock, Le);
						break;

					case FCmpInst::FCMP_OLE:
						processCmp(I, preBlock, postBlock, Le);
						break;
					//FIXME: handle default cases here
					default:
						break;
				}

				break;
			}

			default:
				//B->getInstList().push_back(&I);
				break;
			}
   	  }

    /* Only change the types of floating point variables */
    bool needTochangeType(Value *value){
    	if (
    		value->getType() == PointerType::getDoublePtrTy(value->getContext())
			|| value->getType() == Type::getDoubleTy(value->getContext())
			|| value->getType() == Type::getFloatTy(value->getContext())
			|| value->getType() == PointerType::getFloatPtrTy(value->getContext())
    	)
    		return true;
    	else
    		return false;
    }

    void processUnchange (Function &F){
    	for (Function::iterator BB = F.begin(), BE = F.end(); BB != BE; BB++){
    		BasicBlock *B = BB;
    		for (
    				BasicBlock::iterator II = B->begin(),
    				IE = B->end(); II != IE; II++
    		) {
    			Instruction *I = II;
			if (I->getOpcode() == Instruction::FPExt){
			  II++;
			  I->replaceAllUsesWith(I->getOperand(0));
			  I->eraseFromParent();
			  continue;
			}
    			if (isUnchangeable (I)) 
    				processUnchange(I);
    		}
    	} 
    }

    void processUnchange(Instruction *I){
    	unsigned numOfOps = I->getNumOperands();
    	if (I->getType() == mpq) {
    		AbstractTypeUser::setType( I, Type::getDoubleTy(I->getContext()));
    		AllocaInst *alloc = new AllocaInst(mpq, "",I);
    		Value *init_params[] = {
    				alloc
    		};

    		CallInst *init_call = CallInst::Create(
    			mpq_init, init_params, array_endof(init_params), "", I
    		);
    		I->moveBefore(alloc);
    		Value *set_params[] = {
    		    		alloc,
    		    		I->getUnderlyingObject()
    		};

    		CallInst *set_d_call = CallInst::Create(
    				mpq_set_d, set_params, array_endof(set_params),
    				"", init_call
    		);
    		init_call->moveBefore(set_d_call);
    		LoadInst *load = new LoadInst(alloc,"",set_d_call);
    		set_d_call->moveBefore(load);
    		AbstractTypeUser::setType( I, mpq);
    		I->replaceAllUsesWith(load);
    		// Reset the type and parameter
    		AbstractTypeUser::setType( I, Type::getDoubleTy(I->getContext()));
    		set_d_call->setOperand(2, I->getUnderlyingObject());
    	}
    	for (unsigned i  = 0; i< numOfOps; i++){
    		Value *operand = I->getOperand(i);
    		if (operand->getType() == mpq){
    			AllocaInst *alloc = new AllocaInst(mpq, "",I);
    			Value *init_params[] = {
    					alloc
    			};

    			CallInst::Create(
    				mpq_init, init_params, array_endof(init_params), "", I
    			);
    			new StoreInst (
    					operand, alloc, I
    			);
    			Value *get_params[] = {
    				alloc
    			};
    			CallInst *mpq_get_d_call = CallInst::Create(
    				mpq_get_d, get_params, array_endof(get_params), "", I
    			);
    			I->setOperand (i, mpq_get_d_call);
    		} else if (operand->getType() == mpq_Ptr){
    			Value *get_params[] = {
    				operand
    			};
    			CallInst *mpq_get_d_call = CallInst::Create(
    				mpq_get_d, get_params, array_endof(get_params), "", I
    			);
    			I->setOperand (i, mpq_get_d_call);
    		}
    	}    	
    }

    // Set values back to parameters in function call
    void setParameterBack(
    	Instruction *originalInst, Instruction *I, BasicBlock *BB
    ) {
    	for (unsigned i = 1; i < I->getNumOperands(); i++) {
    		Value *operand = I->getOperand(i);
    		Value *originalOp = originalInst->getOperand(i);
    		if (
    				const PointerType *ptrType = dyn_cast<PointerType> (
    						originalOp->getType()
    				)
    		) {

    			if (originalOp->getType() == mpq_Ptr) {
    				LoadInst *load_operand = new LoadInst(operand, "", BB);
    				mpq_init_call(originalOp,BB);
    				mpq_set_d_call(originalOp,load_operand,BB);

    		   } else {
    		   		//Wrap function with struct parameter
    		   		if (
    		   			const StructType *STy = dyn_cast<StructType> (
    		   					ptrType->getElementType()
    		   			)
    		   		) {
    		   			unsigned numOfElements = STy->getNumElements();
    		    		for (unsigned j = 0; j < numOfElements; j++) {

   							const Type* elemType = STy->getElementType(j);
   							Value *idx[] = {
   									ConstantInt::get(
   											Type::getInt32Ty(I->getContext()), 0
   									),
   									ConstantInt::get(
   											Type::getInt32Ty(I->getContext()), j
   									)
   							};
   							GetElementPtrInst *element =
    		    						GetElementPtrInst::CreateInBounds (
    		    								operand, idx, idx+2, "", BB
    		    						);
    		    			GetElementPtrInst *original_element =
    		    					GetElementPtrInst::CreateInBounds (
    		    							originalOp, idx, idx+2, "", BB
    		    					);
    		    			if (elemType == mpq ) {
    		    				// set the value back to the parameter
    		    				LoadInst *elem_load = new LoadInst(element, "", BB);
    		    				mpq_init_call(original_element,BB);
    		    				mpq_set_d_call(original_element,elem_load,BB);
    		    			} else if (elemType == mpq_Ptr) {
    		    				// set the value back to the parameter
    		    				//CallInst *set_call = mpq_set_d_call(element,alloc,BB);
    		    				//I->moveBefore(set_call);
    		    			} else {

    		    			}
    		    		}

    		   		}

    		   }
    		}
    	}
    }

    // Change the parameters in a function call
    void changeParameters (
    	Instruction *I, BasicBlock *BB,
    	Function *function, Instruction *originalInst
    ) {
    	const FunctionType *originalFnType = function->getFunctionType();
    	// Change the parameters
		for (unsigned i = 1; i < I->getNumOperands(); i++) {
			Value *operand = I->getOperand(i);
			if (
				const PointerType *ptrType = dyn_cast<PointerType> (
					operand->getType()
				)
			) {
				if (operand->getType() == mpq_Ptr) {
					//FIXME: may need to call mpq_init_call here
					AllocaInst *alloc_operand;
					if (function->isVarArg())
						alloc_operand = new AllocaInst (
							Type::getDoubleTy(I->getContext()) , "", BB
						);
					else {

						const Type *originalType = originalFnType->getParamType(i-1);
						const PointerType *originalPtrType = dyn_cast <PointerType> (
							originalType
						);
						alloc_operand = new AllocaInst (
							originalPtrType->getElementType() , "", BB
						);
					}
					CallInst *get_call = mpq_get_d_call(operand, BB);
					new StoreInst (get_call, alloc_operand, BB);
					I->setOperand(i, alloc_operand);

				} else {
					//Wrap function with struct parameter
					if (
						const StructType *STy = dyn_cast<StructType> (
								ptrType->getElementType()
						)
					) {
						const StructType *originalSTy;
						if (function->isVarArg())
							originalSTy= getOriginalType(STy);
						else {

							const Type *originalType = originalFnType->getParamType(i-1);
							const PointerType *originalPtrType = dyn_cast <PointerType> (
									originalType
							);
							originalSTy = dyn_cast<StructType> (
									originalPtrType->getElementType()
							);
						}
						unsigned numOfElements = STy->getNumElements();
						AllocaInst *alloc_operand = new AllocaInst (
							originalSTy , "", BB
						);
						for (unsigned j = 0; j < numOfElements; j++) {

							const Type* elemType = STy->getElementType(j);
							Value *idx[] = {
								ConstantInt::get(
										Type::getInt32Ty(I->getContext()), 0
								),
								ConstantInt::get(
										Type::getInt32Ty(I->getContext()), j
								)
							};
							GetElementPtrInst *element =
								GetElementPtrInst::CreateInBounds (
										operand, idx, idx+2, "", BB
								);
							GetElementPtrInst *element_alloc =
								GetElementPtrInst::CreateInBounds (
										alloc_operand, idx, idx+2, "", BB
								);
							if (elemType == mpq ) {
								//FIXME: should only call init after alloc
								//mpq_init_call (element, BB);
								CallInst *get_call = mpq_get_d_call(element, BB);
								new StoreInst (get_call, element_alloc, BB);

							} else if (elemType == mpq_Ptr) {
								LoadInst *load = new LoadInst(element, "", BB);
								CallInst *get_call = mpq_get_d_call(load, BB);
								AllocaInst *alloc = new AllocaInst(
										Type::getDoubleTy(I->getContext()),"",BB
								);
								new StoreInst (get_call, alloc, BB);
								new StoreInst (alloc, element_alloc, BB);

							} else {
								// FIXME: should restore value here!!
								/*
								LoadInst* load = new LoadInst(element, "", BB);
								new StoreInst (load, element_alloc, BB);*/
							}
						}
						I->setOperand(i, alloc_operand);
					}
				}
			}
			//Change the operand with mpq type, change it to original floating type
			else if (operand->getType() == mpq) {
				AllocaInst *alloc_operand = new AllocaInst (mpq,"",BB);
				mpq_init_call (alloc_operand, BB);
				new StoreInst (
						operand, alloc_operand, BB
				);
				CallInst *get_call = mpq_get_d_call(alloc_operand, BB);
				I->setOperand(i,get_call->getUnderlyingObject());
			}

			else if (
				const StructType *STy = dyn_cast<StructType> (
					operand->getType()
				)
			) {
				const StructType *originalSTy;

				if (function->isVarArg())
					originalSTy = getOriginalType(STy);
				else {
					const Type *originalType = originalFnType->getParamType(i-1);
					originalSTy = dyn_cast<StructType> (originalType);
				}
				unsigned numOfElements = STy->getNumElements();
				AllocaInst *alloc_operand = new AllocaInst (
					originalSTy , "", BB
				);
				for (unsigned j = 0; j < numOfElements; j++) {
					const Type* elemType = STy->getElementType(j);
					Value *idx[] = {
						ConstantInt::get(
							Type::getInt32Ty(I->getContext()), 0
						),
						ConstantInt::get(
							Type::getInt32Ty(I->getContext()), j
						)
					};
					GetElementPtrInst *element =
						GetElementPtrInst::CreateInBounds (
							operand, idx, idx+2, "", BB
						);
					GetElementPtrInst *element_alloc =
						GetElementPtrInst::CreateInBounds (
						alloc_operand, idx, idx+2, "", BB
						);
					if (elemType == mpq ) {
						CallInst *get_call = mpq_get_d_call(element, BB);
						new StoreInst (get_call, element_alloc, BB);
					} else if (elemType == mpq_Ptr) {
						LoadInst* load = new LoadInst(element, "", BB);
						CallInst *get_call = mpq_get_d_call(load, BB);
						new StoreInst (get_call, element_alloc,BB);
					}else {
						LoadInst* load = new LoadInst(element, "", BB);
						new StoreInst (load, element_alloc, BB);
					}
				}
				LoadInst *loadInst = new LoadInst(alloc_operand,"",BB);
				I->setOperand(i,loadInst);
			}

		}
    }

    /// Copy a content of a struct value, add additional instructions
    /// to the current basic block
    Value* copyContent(Value* value, BasicBlock* BB) {
		if (const PointerType* ptrType = dyn_cast<PointerType>(value->getType())) {
			if (const StructType* STy = dyn_cast<StructType>(ptrType->getElementType())) {
				const StructType* newSTy = getNewType(STy);
				AllocaInst *newValue = new AllocaInst(newSTy, "", BB);
				processAllocate(newValue, BB);
				unsigned numOfElements = STy->getNumElements();
				for (unsigned j = 0; j < numOfElements; j++) {
					const Type* elemType = STy->getElementType(j);
					Value *idx[] = {
							ConstantInt::get(
									Type::getInt32Ty(BB->getContext()), 0
							),
							ConstantInt::get(
									Type::getInt32Ty(BB->getContext()), j
							)
					};
					GetElementPtrInst *element =
							GetElementPtrInst::CreateInBounds (
									value, idx, idx+2, "", BB
							);

					GetElementPtrInst *newElement =
							GetElementPtrInst::CreateInBounds (
									newValue, idx, idx+2, "", BB
							);

					if (elemType == Type::getDoubleTy(BB->getContext())) {
						LoadInst* loadElem = new LoadInst(element, "", BB);
						mpq_set_d_call(newElement, loadElem, BB);
					}
					else if (
						elemType == Type::getDoublePtrTy(BB->getContext())
					) {
						LoadInst* loadElem = new LoadInst(element, "", BB);
						LoadInst* newLoadElem = new LoadInst(loadElem, "", BB);
						AllocaInst* newVal = new AllocaInst(mpq, "", BB);
						mpq_init_call(newVal, BB);
						mpq_set_d_call(newVal, newLoadElem, BB);
						new StoreInst (newVal, newElement, BB);
					}
					else if (
						const PointerType* elemPtrType = dyn_cast<PointerType> (
							elemType
						)
					) {
						if (
							elemPtrType->getElementType()->isStructTy()
						) {
							LoadInst* loadElem = new LoadInst(element, "", BB);
							Value* newElemVal = copyContent(loadElem, BB);
							new StoreInst (newElemVal, newElement, BB);
						}
						else {
							LoadInst* load = new LoadInst(element, "", BB);
							new StoreInst (load, newElement, BB);
						}
					}
					else {
						LoadInst* load = new LoadInst(element, "", BB);
						new StoreInst (load, newElement, BB);
					}
				}
				return newValue;
			}
		}
		return value;
    }

    /* Change the types in function calls */
    void processCall(Instruction *I, BasicBlock *BB) {
    	Function *function = dyn_cast<Function> (I->getOperand(0));

    	// Handle external functions
    	if (function->isDeclaration() ) {
    		replaceType (I, BB);
    		Instruction *originalInst = I->clone();
    		changeParameters(I, BB, function, originalInst);
			Value *res = I->getUnderlyingObject();
			const Type *resType = function->getReturnType();
			if (res->getType() == mpq ){
    			if (resType == mpq){
    				resType = Type::getDoubleTy(function->getContext());
    				function->getFunctionType()->setReturnType(resType);
    			}
    			AllocaInst *alloc_res = new AllocaInst (mpq,"",BB);
    			mpq_init_call (alloc_res, BB);
    			Instruction *function_call = I->clone();
    			AbstractTypeUser::setType(function_call->getUnderlyingObject(),resType);  
    			BB->getInstList().push_back(function_call);

    			mpq_set_d_call(
    				alloc_res, function_call->getUnderlyingObject(), BB
    			);
    			LoadInst *load_operand = new LoadInst(alloc_res,"",BB);
    			res->replaceAllUsesWith(load_operand->getUnderlyingObject());
    			AbstractTypeUser::setType(res,resType);
    			I->eraseFromParent();
    		}
    		else if (
    			res->getType() == Type::getDoubleTy(I->getContext())
				|| res->getType() == Type::getFloatTy(I->getContext())
    		) {
    			AllocaInst *alloc_res = new AllocaInst (mpq,"",BB);
    			mpq_init_call(alloc_res, BB);
    			Instruction *function_call = I->clone();
    			BB->getInstList().push_back(function_call);
    			mpq_set_d_call(
    				alloc_res, function_call->getUnderlyingObject(), BB
    			);
    			LoadInst *load_operand = new LoadInst(alloc_res,"",BB);
    			AbstractTypeUser::setType(res,mpq);
    			I->replaceAllUsesWith(load_operand->getUnderlyingObject());
    			I->eraseFromParent();
    		}
    		else if (
    			const StructType* STy = dyn_cast<StructType> ( I->getType())
    		) {
    		  	const Type* newSTy = getNewType(STy);
    		  	I->removeFromParent();
    		  	BB->getInstList().push_back(I);
    		  	/// FIXME: copy the contents to new variable
    		  	AllocaInst *alloc_res = new AllocaInst(newSTy, "", BB);
    		  	LoadInst *load_res = new LoadInst(alloc_res, "", BB);
    		    AbstractTypeUser::setType(res, newSTy);
    		    I->replaceAllUsesWith(load_res);
    		    AbstractTypeUser::setType(res, STy);
    		}
    		else if (
    			const PointerType *ptrType = dyn_cast<PointerType> (I->getType())
    		) {
    			if (
    				const StructType* STy = dyn_cast<StructType> (
    					ptrType->getElementType()
    			    )
    			) {
    				const Type* newSTy = getNewType(STy);
    				const Type* newPtrSTy = PointerType::getUnqual(newSTy);
    				I->removeFromParent();
    				BB->getInstList().push_back(I);
    				AllocaInst *alloc_res = new AllocaInst(newSTy, "", BB);
    				AbstractTypeUser::setType(res, newPtrSTy);
    				I->replaceAllUsesWith(alloc_res);
    				AbstractTypeUser::setType(res, resType);
    				Value* newValue = copyContent(res, BB);
    				alloc_res->getType()->dump();
    				newValue->getType()->dump();
    				LoadInst *loadValue = new LoadInst(newValue, "", BB);
    				new StoreInst(loadValue, alloc_res, "", BB);
    		    }
    			else {
    				I->removeFromParent();
    				BB->getInstList().push_back(I);
    				// Set back values to parameter if necessary
    				setParameterBack(originalInst, I, BB);
    			}
    		}
    		else {
    			I->removeFromParent();
    			BB->getInstList().push_back(I);
    			// Set back values to parameter if necessary
    			setParameterBack(originalInst, I, BB);
    		}
    		//Hacky: add original Inst to BB then remove it
    		BB->getInstList().push_back(originalInst);
    		originalInst->eraseFromParent();
    	} else {
    		replaceType (I, BB);
    		I->removeFromParent();
    		BB->getInstList().push_back(I);		
    	}
    }

    // Replace all constants in a function
    // FIXME: should replace ArrayConst and StructConst
    void replaceConstants (Function &F){
    	if (F.isDeclaration())
    		return;
    	BasicBlock *entryBB = &F.getEntryBlock();
    	BasicBlock *constantBB = BasicBlock::Create(
    			F.getContext(), "", &F, entryBB
    	);
    	for (Function::iterator BB = F.begin(), BE = F.end(); BB!= BE; BB++) {
    		BasicBlock *B = BB;
    		for (
    				BasicBlock::iterator II = B->begin(),
    				IE = B->end(); II != IE; II++
    		) {
    			Instruction *I = II;
    			int numOp = I->getNumOperands();

    			for (int i= 0; i < numOp; i++){
    				Value* val = I->getOperand(i);
    				if (
    					val->getType() == Type::getDoubleTy(val->getContext()) ||
    					val->getType() == Type::getFloatTy(val->getContext())
    				) {
    					if (ConstantFP *C = dyn_cast<ConstantFP> (val)){
    						//Set type to double
    						AbstractTypeUser::setType(
    								val,Type::getDoubleTy(val->getContext())
    						);
    						AllocaInst *alloc_Val = new AllocaInst(
    								mpq, "", constantBB
    						);
    						mpq_init_call(alloc_Val,constantBB);
    						mpq_set_d_call(alloc_Val, C, constantBB);
    						LoadInst *load_Val = new LoadInst(
    							alloc_Val->getUnderlyingObject(),
    							"", constantBB
    						);
    						I->setOperand(i,load_Val);
							continue;
    					}
    				}
    			}
    		}
    	}
    	BranchInst *br = BranchInst::Create(entryBB);
    	constantBB->getInstList().push_back(br);

    }

    /// We only replace the expression for arithmetic operations
    /// FIXME: replace the constant values.
    Value* replaceConstantExpr(
    	BasicBlock *BB, ConstantExpr* constantExpr
    ) {
    	Value *operand1 = constantExpr->getOperand(0);
    	Value *operand2 = constantExpr->getOperand(1);

    	if (operand1->getType() == Type::getDoubleTy(BB->getContext())) {
    		AbstractTypeUser::setType (operand1, mpq);
    	}

    	if (operand2->getType() == Type::getDoubleTy(BB->getContext())) {
    		AbstractTypeUser::setType (operand2, mpq);
    	}

    	if (ConstantExpr *C = dyn_cast<ConstantExpr>(operand1)) {
    		switch (C->getOpcode()) {
				case Instruction::FAdd:
				case Instruction::FSub:
				case Instruction::FMul:
				case Instruction::FDiv:
					operand1 = replaceConstantExpr (BB, C);
					break;
    		}
    	}

    	if (ConstantExpr *C = dyn_cast<ConstantExpr>(operand2)) {
    		switch (C->getOpcode()) {
				case Instruction::FAdd:
				case Instruction::FSub:
				case Instruction::FMul:
				case Instruction::FDiv:
					operand2 = replaceConstantExpr (BB, C);
					break;
    		}
    	}

    	if (ConstantFP *C = dyn_cast<ConstantFP> (operand1)){
    		//Set type to double
    		AbstractTypeUser::setType(
    				operand1, Type::getDoubleTy(BB->getContext())
    		);
    		AllocaInst *alloc_Val = new AllocaInst(
    				mpq, "", BB
    		);
    		mpq_init_call(alloc_Val, BB);
    		mpq_set_d_call(alloc_Val, C, BB);
    		LoadInst *load_Val = new LoadInst(
    				alloc_Val->getUnderlyingObject(),
    				"", BB
    		);
    		operand1 = load_Val;
    	}

    	if (ConstantFP *C = dyn_cast<ConstantFP> (operand2)){
    		//Set type to double
    		AbstractTypeUser::setType(
    			operand2, Type::getDoubleTy(BB->getContext())
    		);
    		AllocaInst *alloc_Val = new AllocaInst(
    			mpq, "", BB
    		);
    	    mpq_init_call(alloc_Val, BB);
    	    mpq_set_d_call(alloc_Val, C, BB);
    	    LoadInst *load_Val = new LoadInst(
    	    	alloc_Val->getUnderlyingObject(),
    	    	"", BB
    	    );
    	    operand2 = load_Val;
    	}

    	AllocaInst *alloc_res = new AllocaInst(mpq, "", BB);
    	mpq_init_call(alloc_res, BB);
    	AllocaInst *alloc_op1 = new AllocaInst(mpq, "", BB);
    	mpq_init_call(alloc_op1, BB);
    	AllocaInst *alloc_op2 = new AllocaInst(mpq, "", BB);
    	mpq_init_call(alloc_op2, BB);

    	new StoreInst(
    		operand1, alloc_op1->getUnderlyingObject(), BB
    	);

    	new StoreInst(
    		operand2, alloc_op2->getUnderlyingObject(), BB
    	);

    	CallInst *op_call;
    	switch(constantExpr->getOpcode()){
    	   	case Instruction::FAdd:
    	  		op_call = mpq_add_call(
    				alloc_res->getUnderlyingObject(),
    				alloc_op1->getUnderlyingObject(),
    	    		alloc_op2->getUnderlyingObject(),
    	    		BB
    	    	);
    	  		break;

    	   	case Instruction::FSub:
    	   		op_call = mpq_sub_call(
    	   			alloc_res->getUnderlyingObject(),
    	    		alloc_op1->getUnderlyingObject(),
    	    		alloc_op2->getUnderlyingObject(),
    	    		BB
    	  	    );
    	  	    break;
    	   case Instruction::FMul:
    	       	op_call = mpq_mul_call(
    	   			alloc_res->getUnderlyingObject(),
    	   			alloc_op1->getUnderlyingObject(),
    	   			alloc_op2->getUnderlyingObject(),
    	   			BB
    	       	);
    	      	break;
    	   case Instruction::FDiv:
    	       	op_call = mpq_div_call(
    	   			alloc_res->getUnderlyingObject(),
    	   			alloc_op1->getUnderlyingObject(),
    	   			alloc_op2->getUnderlyingObject(),
    	   	   		BB
    	   	   	);
    	   	   	break;
    	}
    	LoadInst *load_res = new LoadInst(alloc_res, "", BB);
    	return load_res;
    }

    // Search all floating point type in the instruction and replace by GMP type
    void replaceType (Instruction *I, BasicBlock* BB){

    	int numOp = I->getNumOperands();

    	if (I->getOpcode() == Instruction::Alloca) {
    		if (I->getType() == Type::getDoublePtrTy(I->getContext()))
    			AbstractTypeUser::setType(I, mpq_Ptr);
    	}

    	for (int i= 0; i < numOp; i++) {
    		//Handle the case the type of operand is struct type
    		Value *val = I->getOperand(i);

    		if (
    			ConstantExpr *constantExpr = dyn_cast<ConstantExpr>(val)
    		) {
    			switch(constantExpr->getOpcode()){
					case Instruction::FAdd:
					case Instruction::FSub:
					case Instruction::FMul:
					case Instruction::FDiv:
						Value* newVal = replaceConstantExpr(BB, constantExpr);
						I->setOperand(i, newVal);
						break;
    			}
    		}

    		// In case the instruction is function call then do not change the type of the function
    		if (i == 0 && I->getOpcode() == Instruction::Call) {
    			continue;
    		}

    		if (val->getType() == PointerType::getDoublePtrTy(val->getContext()) ||
    			val->getType() == PointerType::getFloatPtrTy(val->getContext())) {
    			AbstractTypeUser::setType(val,mpq_Ptr);
    		} else if (val->getType() == Type::getDoubleTy(val->getContext()) ||
    				val->getType() == Type::getFloatTy(val->getContext())) {
    			// FIXME: zero constants have problems ?
    			if (ConstantFP *C = dyn_cast<ConstantFP> (val)) {
    				AllocaInst *alloc_Val = new AllocaInst(
    						mpq, "", BB
    				);
    				mpq_init_call (alloc_Val, BB);
    				mpq_set_d_call(alloc_Val, C, BB);
    				LoadInst *load_Val = new LoadInst(
    						alloc_Val->getUnderlyingObject(),"" , BB
    				);
    				I->setOperand(i, load_Val);
    			} else
    				AbstractTypeUser::setType(val, mpq);
    		}
    		else if (
    			const PointerType *ptrType = dyn_cast<PointerType> (
    				val->getType()
    			)
    		) {
    			if (const ArrayType *aty = dyn_cast<ArrayType> (ptrType->getElementType())){
    				if (
    						aty->getElementType() == Type::getDoubleTy(I->getContext()) ||
    						aty->getElementType() == Type::getFloatTy(I->getContext())
    				) {
    					ArrayType *newArrayType = ArrayType::get(
    							mpq,aty->getNumElements()
    					);
    					Type *newArrayPtrType = PointerType::getUnqual(
    							newArrayType
    					);
    					AbstractTypeUser::setType(val, newArrayPtrType);
    				}
    			}

    			if (
    				const StructType *STy = dyn_cast<StructType> (
    						ptrType->getElementType()
    				)
    			) {
    				StructType *newStructType = getNewType(STy);
    				const Type *newPtrType = PointerType::getUnqual(
    						newStructType
    				);
    				AbstractTypeUser::setType(val, newPtrType);

    			}
    			// The case double**
    			else if (
    				ptrType->getElementType() == PointerType::getDoublePtrTy(
    					val->getContext()) ||
    				 ptrType->getElementType() == PointerType::getFloatPtrTy(
						val->getContext())
    			) {
    				const Type *double_mpq_Ptr = PointerType::getUnqual(
    						mpq_Ptr
    				);
    				AbstractTypeUser::setType(val,double_mpq_Ptr);
    				
    			}
    			//The case struct**
    			else if (
    					const PointerType* refOfPtrType = dyn_cast<PointerType>(
    							ptrType->getElementType()
    					)
    			) {
    				if (
    					const StructType* STy = dyn_cast<StructType> (
    							refOfPtrType->getElementType()
    					)
    				) {
    					const Type* newSTy = getNewType(STy);
    					const Type* newPtrSTy = PointerType::getUnqual(newSTy);
    					const Type* newTy = PointerType::getUnqual(newPtrSTy);
    					AbstractTypeUser::setType(val,newTy);
    				}
    			}
    		}
    		else if (
    				const ArrayType *aty = dyn_cast<ArrayType> (
    				val->getType())
			) {
    			if (
    				aty->getElementType() == Type::getDoubleTy(I->getContext()) ||
    				aty->getElementType() == Type::getFloatTy(I->getContext())
    			) {
    				ArrayType *newArrayType = ArrayType::get(
    						mpq,aty->getNumElements()
    				);
    				AbstractTypeUser::setType(val, newArrayType);
    			}
    		}
    		else if (
    				const StructType *STy = dyn_cast<StructType> (
    						val->getType()
    				)
    		) {
    			const StructType* newStructType = getNewType(STy);
    			AbstractTypeUser::setType(val,newStructType);    			
    		}
    	}

    }

    void processAllocate(Instruction *I, BasicBlock *BB) {
    	Value *op = I->getUnderlyingObject();
    	if (I->getType() == mpq_Ptr) {
    		mpq_init_call(I, BB);
    	}
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
    			const StructType* newSTy = getNewType(STy);
    	    	const Type* newPtrSTy = PointerType::getUnqual(newSTy);
    	    	AbstractTypeUser::setType(op,newPtrSTy);
    	    	unsigned numOfElements = newSTy->getNumElements();
    	    	for (unsigned i = 0; i < numOfElements; i++) {
    	    		const Type* elemType = newSTy->getElementType(i);
    	    		if (elemType == mpq) {
    	    				Value *idx[] = {
    	    						ConstantInt::get(
    	    								Type::getInt32Ty(I->getContext()), 0
    	    						),
    	    						ConstantInt::get(
    	    								Type::getInt32Ty(I->getContext()), i
									)
    	    				};
    	    				GetElementPtrInst *element =
    	    						GetElementPtrInst::CreateInBounds (
    	    								op, idx, idx+2, "", BB
    	    						);
							mpq_init_call (element, BB);
    	    		}
    	    	}
    		}
    	}


    }


    /*
     * Traverse all PHI nodes in the function and
     * change the references in the instruction
     */
    void replacePHIReferences(BasicBlock* oldBB, BasicBlock *newBB){
    	Function * function = oldBB->getParent();
    	Function::iterator BB = function->begin();
    	BasicBlock *B = BB;
    	while ( BB != function->end()){
    		BasicBlock *next = ++BB;
    		for (BasicBlock::iterator II = B->begin(); II != B->end(); II++){
    			Instruction *I = II;
    			//Detect phi instructions in basic block
    			if (PHINode *phi = dyn_cast <PHINode> (I) ){
    				unsigned numOfVals = phi->getNumIncomingValues();
    				for (unsigned i = 0; i < numOfVals; i++){
    					if (phi->getIncomingBlock(i) == oldBB)
    						phi->setIncomingBlock( i, newBB );
    				}
    			}
    		}
    		B = next;
    	}
    }

    /* Traverse a basic block, for each instruction transform into a list of
     * basic blocks if needed
     */

    void processBlock(BasicBlock *B, BasicBlock *nextBB){

    	BasicBlock *preBlock = BasicBlock::Create(
    			B->getContext(),"", B->getParent(), B
    	);
    	//Set reference to B to the reference to preBlock
    	B->replaceAllUsesWith(preBlock);
    	BasicBlock *firstBB = preBlock;
    	BasicBlock::iterator II = B->begin();

    	while (II != B->end()){
    		Instruction *I = II;
    		Instruction *next = ++II;
    		//Transform the instruction if needed
    		if (needToTransform(I)) {
    			BasicBlock *postBlock = BasicBlock::Create(
    					preBlock->getContext(),"",
    					preBlock->getParent(), nextBB
    			);
    			postBlock->moveBefore(nextBB);
    			processInstruction(I, preBlock,postBlock);
    			preBlock = postBlock;
			}
    		// Change the types and add the instruction to preBlock
    		else{
    			if (I->getOpcode()==Instruction::Call){
    				processCall(I, preBlock);
    			}
    			// Handle Alloca Instructions
    			else if (I->getOpcode() == Instruction::Alloca) {
    				replaceType(I, preBlock);
    				I->removeFromParent();
    				preBlock->getInstList().push_back(I);
    				processAllocate(I, preBlock);
    			}
    			else {
    				replaceType(I, preBlock);
    				I->removeFromParent();
    				preBlock->getInstList().push_back(I);				
    			}
    		}    		
    		I = next;
		}
    	// Set the references in PHI nodes for B by preBlock
    	replacePHIReferences(firstBB, preBlock);
    	B->removeFromParent();
    }

    /* Only change the types of floating point arithmetic instructions */
    /* Change types of floating point comparison */
    bool needToTransform(Instruction *I){
    	switch ( I->getOpcode()) {
			case Instruction::FAdd:
				return true;
			case Instruction::FSub:
				return true;
			case Instruction::FMul:
				return true;
			case Instruction::FDiv:
				return true;
			case Instruction::FCmp:
				return true;
			default:
				return false;
    	}
    }

    /*
     * Some instructions need to be unchanged in the types     *
     */
    bool isUnchangeable(Instruction *I){
    	switch (I->getOpcode()){
    	case Instruction::UIToFP:
    		return true;
    	case Instruction::SIToFP:
    		return true;
    	case Instruction::FPToSI:
    		return true;
    	case Instruction::FPToUI:
    		return true;
    	case Instruction::FPExt:
    		return true;
    	default:
    		return false;
    	}
    }

    // Change the signature of function defined in module
    void changeFunctionSignature(Function &F){

      if (F.getName().compare("main") == 0 ||
    			F.getName().startswith("setConstants"))
	    return;
      if (F.isDeclaration())
	    return;
      std::vector<const Type *> typeParams;
      const Type *resultType;
      // Change struct type in function signature
    	if (
    		const PointerType *ptrType = dyn_cast<PointerType> (
    				F.getReturnType()
    		)
    	) {

    		if (
    			const StructType *STy = dyn_cast<StructType> (
    				ptrType->getElementType()
    			)
    		) {
    			StructType* newStructType = getNewType(STy);
    			const Type *newPtrType = PointerType::getUnqual(
    					newStructType
    			);
    			resultType = newPtrType;
    		} // Change return Type
    		else if (
    			F.getReturnType() == PointerType::getDoublePtrTy(F.getContext()) ||
    			F.getReturnType() == PointerType::getFloatPtrTy(F.getContext())
    		) {
    			resultType = mpq_Ptr;
    		} else {
    			resultType = F.getReturnType();
    		}
    	} else if (
    			const StructType *STy = dyn_cast<StructType> (
    	    		F.getReturnType()
    			)
    	) {
    		StructType* newStructType = getNewType(STy);
    		resultType = newStructType;
    	} else if (F.getReturnType() == Type::getDoubleTy(F.getContext()) ||
    			F.getReturnType() == Type::getFloatTy(F.getContext())) {
    		resultType = mpq;
    	} else {
    		resultType = F.getReturnType();
    	}

    	// Change types of parameters
    	unsigned paramNum = F.getFunctionType()->getNumParams();

    	Function::arg_iterator argument = F.getArgumentList().begin();
    	for (unsigned i = 0 ; i < paramNum; i++) {
    		const Type* paramType = F.getFunctionType()->getParamType(i);
    		Value *param = argument;
    		//Change struct type
    		if (
    			const PointerType *ptrType = dyn_cast<PointerType> (
    				F.getFunctionType()->getParamType(i)
    			)
    		) {

    			if (
    				const StructType *STy = dyn_cast<StructType> (
    					ptrType->getElementType()
    				)
    			) {
    				StructType* newStructType = getNewType(STy);
    				const Type *newPtrType = PointerType::getUnqual(
    						newStructType
    				);
    				AbstractTypeUser::setType(param, newPtrType);
    				typeParams.push_back(newPtrType);
    			} else if (F.getFunctionType()->getParamType(i)
    				== PointerType::getDoublePtrTy(F.getContext())
					|| F.getFunctionType()->getParamType(i)
					== PointerType::getFloatPtrTy(F.getContext())){
    				AbstractTypeUser::setType(param, mpq_Ptr);
    				typeParams.push_back(mpq_Ptr);
			} else {
				typeParams.push_back(paramType);
			}

    		} else if (
    			const StructType *STy = dyn_cast<StructType> (
    				F.getFunctionType()->getParamType(i)
    			)
    		) {
    			const StructType* newStructType = getNewType(STy);
    			typeParams.push_back(newStructType);
    			AbstractTypeUser::setType(param, newStructType);
    		} else if (F.getFunctionType()->getParamType(i)
    				== Type::getDoubleTy(F.getContext()) ||
    			   F.getFunctionType()->getParamType(i)
					== Type::getFloatTy(F.getContext())) {
    			typeParams.push_back(mpq);
    			AbstractTypeUser::setType(param, mpq);
    		} else {
    			typeParams.push_back(paramType);
    		}
    		argument++;
    	}

    	FunctionType *newFunctionType = FunctionType::get(
    			resultType,typeParams,F.isVarArg()
    	);
    	Type *newType = PointerType::getUnqual(newFunctionType);
    	AbstractTypeUser::setType(&F,newType);
      
    }
    
    bool runOnFunction(Function &F) {

    	// If the function is not defined in module, do not change the types
    	if ( F.isDeclaration()){
    		return false;
    	}
    	// Ignore added function
    	// FIXME: need to handle main adder, process main also
    	//if (F.getName().compare("main") == 0 ||
    	//		F.getName().compare("setConstants") == 0 )
    	if (F.getName().startswith("setConstants") ||
    		F.getName().startswith("no_"))
    		return false;
    	changeFunctionSignature(F);
    	replaceConstants(F);
		
    	Function::iterator BB = ++F.begin(); // Passing the constant BB
    	BasicBlock *B = BB;
    	while ( BB != F.end()) {
    		BasicBlock *next = ++BB;
    		processBlock(B,next);
    		B = next;
    	}
    	processUnchange(F);

    	return true;
    }

    /* Initialize the function calls and the types */
    bool doInitialization(Module &M){

      mpq = dyn_cast<StructType> (M.getTypeByName("struct.__mpq_struct"));
      mpq_Ptr = dyn_cast <Type> (PointerType::getUnqual(mpq));

      //Declare mpq_init function
      mpq_init = cast<Function>(
    		  M.getOrInsertFunction("__gmpq_init", Type::getVoidTy(M.getContext()),
    		  PointerType::getUnqual(mpq), NULL)
      );

      mpq_set_d = cast<Function>(
    		  M.getOrInsertFunction("__gmpq_set_d", Type::getVoidTy(M.getContext()),
    		  PointerType::getUnqual(mpq), Type::getDoubleTy(M.getContext()), NULL)
      );

      mpq_add = cast<Function>(
    		  M.getOrInsertFunction("__gmpq_add", Type::getVoidTy(M.getContext()),
    		  PointerType::getUnqual(mpq),
    		  PointerType::getUnqual(mpq),
    		  PointerType::getUnqual(mpq), NULL)
      );

      mpq_sub = cast<Function>(
    		  M.getOrInsertFunction("__gmpq_sub", Type::getVoidTy(M.getContext()),
    		  PointerType::getUnqual(mpq),
    		  PointerType::getUnqual(mpq),
    		  PointerType::getUnqual(mpq), NULL)
      );

      mpq_mul = cast<Function>(
    		  M.getOrInsertFunction("__gmpq_mul", Type::getVoidTy(M.getContext()),
    		  PointerType::getUnqual(mpq),
    		  PointerType::getUnqual(mpq),
    		  PointerType::getUnqual(mpq), NULL)
      );

      mpq_div = cast<Function>(
    		  M.getOrInsertFunction("__gmpq_div", Type::getVoidTy(M.getContext()),
    		  PointerType::getUnqual(mpq),
    		  PointerType::getUnqual(mpq),
    		  PointerType::getUnqual(mpq), NULL)
      );

      mpq_clear = cast<Function>(
    		  M.getOrInsertFunction(
    				  "__gmpq_clear", Type::getVoidTy(M.getContext()
    		  ),
          	  PointerType::getUnqual(mpq), NULL)
      );

      mpq_get_d = cast<Function>(
    		  M.getOrInsertFunction(
    				  "__gmpq_get_d", Type::getDoubleTy(M.getContext()
    		  ),
          	  PointerType::getUnqual(mpq), NULL)
      );

      mpq_cmp = cast<Function>(
    		  M.getOrInsertFunction(
    				  "__gmpq_cmp", Type::getInt32Ty(M.getContext()
          	  ),
              PointerType::getUnqual(mpq), PointerType::getUnqual(mpq), NULL)
      );

      setGlobals = cast<Function>(M.getOrInsertFunction (
    		  "setGlobals", Type::getVoidTy(M.getContext()), NULL)
      );
      setGlobals->setLinkage(GlobalValue::PrivateLinkage);
      setGlobalsBB = BasicBlock::Create(
    		  M.getContext(), "", setGlobals
      );
      setGlobalsTeminator = ReturnInst::Create(M.getContext(), setGlobalsBB);

      std::vector<Constant*> StructInit;
      std::vector<Constant*> ArrayInit;
      StructInit.resize(2);
      StructInit[0] = ConstantInt::get(IntegerType::getInt32Ty(M.getContext()), 65535);
      StructInit[1] = setGlobals;
      Constant *Struct =
    	  ConstantStruct::get(M.getContext(), StructInit, true);
      ArrayInit.push_back(Struct);
      Constant *Array =
          ConstantArray::get(
        		  ArrayType::get(ArrayInit[0]->getType(), ArrayInit.size()),
                  ArrayInit
          );

      new GlobalVariable(
    		  M, Array->getType(),	true,
    		  GlobalValue::AppendingLinkage,
    		  Array, "llvm.global_ctors"
      );

      return true;

    }

    // Print LLVM output of the module for debugging
    bool doFinalization(Module &M){
      //ReturnInst::Create(M.getContext(), setConstantsBB);
      M.print(errs(),&w); //debugging
      return true;
    }

  };
}

char ArbitraryPrecision::ID = 0;
static RegisterPass<ArbitraryPrecision> Z("ArbitraryPrecision", "Arbitrary Precision Pass");
