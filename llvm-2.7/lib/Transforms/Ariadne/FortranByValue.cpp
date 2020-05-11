//===----------------------------------------------------------------------===//
//
// This file implements the transformation methods to change the function definition
// in fortran code to calling by values rather than calling by references
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ariadne"
#include <float.h>
#include <math.h>
#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Function.h"
#include "llvm/BasicBlock.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Assembly/Writer.h"
#include "llvm/Assembly/AsmAnnotationWriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Instructions.h"
#include "llvm/Intrinsics.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "Ariadne.h"
#include "limits.h"
#include <sstream>
#include <string>
#include <fstream>

using namespace llvm;

namespace {
  struct FortranByValue : public ModulePass {

    static char ID; // Pass identification, replacement for typeid
    FortranByValue() : ModulePass(&ID) {}
    AssemblyAnnotationWriter w; //debugging

    virtual bool runOnModule(Module &M) {

    	for (
    			Module::global_iterator G = M.global_begin(),
    			GE = M.global_end(); G != GE; ++G
    	){
    		GlobalVariable *glVar = G;
    		runOnGlobal (glVar);
    	}

    	for (Module::iterator FF = M.begin(), FE = M.end(); FF != FE; FF++) {
    		Function *F = FF;
    		//Ignore fabs functions
    		if (F->getName().startswith("fabs"))
    			continue;
    		if (!F->isDeclaration())
    			changeFunctionPrototype(F);
    	}
    	M.print(errs(), &w);
    	return true;
    }
    bool isFloatStruct(const StructType *STy) {

    	bool res = false;
    	for (
    			StructType::element_iterator EB = STy->element_begin(),
    			EI = EB, EE = STy->element_end(); EI != EE; ++EI
    	) {
    		const Type *ty = EI->get();

    		if (ty == Type::getFloatTy(STy->getContext()))
    			return true;
    		else if (const StructType *sty = dyn_cast<StructType> (ty) ) {
    			res = isFloatStruct(sty);
    			if (res)
    				return res;
    		}
    		else if (const ArrayType *aty = dyn_cast<ArrayType> (ty)) {
    			if (
    					aty->getElementType() == Type::getFloatTy(STy->getContext())
    			)
    				return true;
    			else if ( ty == Type::getFloatTy(STy->getContext()))
    				return true;
    			else if ( ty == Type::getFloatPtrTy(STy->getContext()))
    	        	return true;
    			else if (const PointerType *ptrTy = dyn_cast<PointerType>(ty)){
    				if (
    						const StructType *sty = dyn_cast<StructType>(
    								ptrTy->getElementType()
    						)
    				) {
    					res = isFloatStruct(sty);
    					if (res)
    						return res;
    				}
    			}
    		}
    	}
    	return res;
    }
    StructType* getNewType(const StructType* STy){

    	std::vector<const Type *> typeParams;
    	for (
    			StructType::element_iterator EB = STy->element_begin(),
					EI = EB, EE = STy->element_end(); EI != EE; ++EI
    	) {
        		const Type *ty = EI->get();
        		// FIXME: The case pointer to a struct type

        		if (const StructType *sty = dyn_cast<StructType> (ty) )
        			typeParams.push_back(getNewType(sty));
        		else if (const ArrayType *aty = dyn_cast<ArrayType> (ty)){
        			if (
        					aty->getElementType() == Type::getFloatTy(STy->getContext())
        			) {
        				ArrayType *newArrayType = ArrayType::get(
        						Type::getDoubleTy(STy->getContext()),aty->getNumElements()
        				);
        				typeParams.push_back(newArrayType);
        			}else {
        				typeParams.push_back(ty);
        			}
        		}
        		else if ( ty == Type::getFloatTy(STy->getContext()))
        			typeParams.push_back(Type::getDoubleTy(STy->getContext()));
        		else if ( ty == Type::getFloatPtrTy(STy->getContext()))
        			typeParams.push_back(Type::getDoublePtrTy(STy->getContext()));
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
        			STy->getContext(),typeParams, STy->isPacked()
        	);

        	return newStructType;
    }

    void replaceType (Instruction *I, Value *val, int i) {
    	Type *ptrToPtrFloat = PointerType::getUnqual(
    			Type::getFloatPtrTy(I->getContext())
    	);
    	Type *ptrToPtrDouble = PointerType::getUnqual(
    			Type::getDoublePtrTy(I->getContext())
    	);
    	if (val->getType() == ptrToPtrFloat)
    		AbstractTypeUser::setType(
    				val, ptrToPtrDouble
    		);
    	else if ( val->getType() == PointerType::getFloatPtrTy(val->getContext())) {
    		AbstractTypeUser::setType(
    				val, Type::getDoublePtrTy(val->getContext())
    		);
    	} else if (	val->getType() == Type::getFloatTy(val->getContext())) {
    		if (
    				ConstantFP* constVal = dyn_cast<ConstantFP>(
    						val
    				)
    		) {
    			double value = constVal->getValueAPF().convertToFloat();
    			Constant* newVal = ConstantFP::get(
    					Type::getDoubleTy(I->getContext()),
    					value
    			);
    			if (i >= 0)
    				I->setOperand( i, newVal);
    		}
    		AbstractTypeUser::setType (
    				val, Type::getDoubleTy(val->getContext())
    		);
    	}
    	else if (
    			const StructType *STy = dyn_cast<StructType> (
    					val->getType()
    			)
    	) {
    		StructType *newStructType = getNewType(STy);
    		AbstractTypeUser::setType(val, newStructType);
		}
    	//FIME: need to handle the array type
    	// Handle pointer type
    	else if (
    			const PointerType *ptrType = dyn_cast<PointerType> (
    					val->getType()
    			)
    	) {
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
    		else if (const ArrayType *aty = dyn_cast<ArrayType> (ptrType->getElementType())){
    			if (
    					aty->getElementType() == Type::getFloatTy(I->getContext())
    			) {
    				ArrayType *newArrayType = ArrayType::get(
    						Type::getDoubleTy(I->getContext()),
    						aty->getNumElements()
    				);
    				Type *newArrayPtrType = PointerType::getUnqual(
    						newArrayType
    				);
    				AbstractTypeUser::setType(val, newArrayPtrType);
    			} else if (
    					const StructType *STy = dyn_cast<StructType> (
    							aty->getElementType()
    					)
    			) {
    				const StructType *newStructType = getNewType(STy);
    				ArrayType *newArrayType = ArrayType::get(
    						newStructType,
    						aty->getNumElements()
    				);
    				Type *newArrayPtrType = PointerType::getUnqual(
    						newArrayType
    				);
    				AbstractTypeUser::setType(
    						val, newArrayPtrType
    				);
    			}
    		} else if (
    				const PointerType *newptrType = dyn_cast<PointerType> (
    						ptrType->getElementType()
    				)
    		) {
    			if (const ArrayType *aty = dyn_cast<ArrayType> (newptrType->getElementType())){
    				if (
    						aty->getElementType() == Type::getFloatTy(I->getContext())
    				) {
    					ArrayType *newArrayType = ArrayType::get(
    							Type::getDoubleTy(I->getContext()),
    							aty->getNumElements()
    					);
    					Type *newArrayPtrType = PointerType::getUnqual(
    							newArrayType
    					);
    					AbstractTypeUser::setType(
    							val, PointerType::getUnqual(newArrayPtrType)
    					);
    				} else if (
    						const StructType *STy = dyn_cast<StructType> (
    								aty->getElementType()
    						)
    				) {
    					const StructType *newStructType = getNewType(STy);
    					ArrayType *newArrayType = ArrayType::get(
    							newStructType,
    							aty->getNumElements()
    					);
    					Type *newArrayPtrType = PointerType::getUnqual(
    							newArrayType
    					);
    					AbstractTypeUser::setType(
    							val, PointerType::getUnqual(newArrayPtrType)
    					);
    				}
    			}else if (
    				const StructType *STy = dyn_cast<StructType> (
    						newptrType->getElementType()
    				)
    			) {
    				StructType *newStructType = getNewType(STy);
    				const Type *newPtrType = PointerType::getUnqual(
    						newStructType
    				);
    				AbstractTypeUser::setType(val, PointerType::getUnqual(newPtrType));
    			}
    		}
    	}
    }

    // Change all float type to double
    void replaceType (Instruction *I) {

    	int numOp = I->getNumOperands();
    	for (int i= 0; i < numOp; i++) {
    		//Handle the case the type of operand is struct type
    		Value *val = I->getOperand(i);
    		replaceType(I, val, i);
    	}
    	replaceType(I, I, -1);
    }

    /* Traverse all globals and change the types, initilizers to gmp_q types */
    void runOnGlobal ( GlobalVariable *G) {

    	const PointerType *ptrType = dyn_cast<PointerType> (G->getType());
    	if (
    			const StructType *STy = dyn_cast <StructType>(
    					ptrType->getElementType()
    			)
    	) {
    		handleStructGlobal(G, STy);
    	}
    	// Handle floating point globals
    	else if ( G->getType() == PointerType::getFloatPtrTy(G->getContext())){
    		if (
    				ConstantFP* val = dyn_cast<ConstantFP>(
    						G->getInitializer()
    				)
    		) {
    			double value = val->getValueAPF().convertToFloat();
    			Constant* newVal = ConstantFP::get(
    					Type::getDoubleTy(G->getContext()),
    					value
    			);
    			AbstractTypeUser::setType(G, Type::getDoublePtrTy(G->getContext()));
    			AbstractTypeUser::setType(
    			    				G->getInitializer(), Type::getDoubleTy(G->getContext())
    			);
    			G->setInitializer(newVal);
    		} else {
    			AbstractTypeUser::setType(G, Type::getDoublePtrTy(G->getContext()));
    			AbstractTypeUser::setType(
    				G->getInitializer(), Type::getDoubleTy(G->getContext())
    			);
    		}
    	}
    	// Handle array type for globals
    	else if (
    			const ArrayType *ATy = dyn_cast <ArrayType>(
    					ptrType->getElementType()
    			)
    	) {
    		handleArrayGlobal(G,ATy);
        }
    }

    // Traverse all struct type and recursively change types in the struct
    void handleStructGlobal (
    		GlobalVariable *G, const StructType *STy
    ) {

    	const Type *newSTy = getNewType(STy);

        Value *initializer = G->getInitializer();

        if (
        	ConstantStruct *structConst = dyn_cast<ConstantStruct> (
        			initializer
        	)
        ) {
        	unsigned numOfOps = structConst->getNumOperands();
        	for (unsigned i = 0; i < numOfOps; i++) {
        		Value *operand = structConst->getOperand(i);
        		if (operand->getType() == Type::getFloatTy(G->getContext())) {
        			if (
        					ConstantFP* val = dyn_cast<ConstantFP>(
        							operand
        					)
        			) {
        				double value = val->getValueAPF().convertToFloat();
        				Constant* newVal = ConstantFP::get(
        						Type::getDoubleTy(G->getContext()),
        						value
        				);
        				structConst->setOperand(i, newVal);
        			}
        			AbstractTypeUser::setType (
        				operand, Type::getDoubleTy(G->getContext())
        			);

        		}
        		else if (operand->getType() == Type::getFloatPtrTy(G->getContext()))
        			AbstractTypeUser::setType (
        					operand, Type::getDoublePtrTy(G->getContext())
        			);
        		else if (
        			const ArrayType *ATy = dyn_cast <ArrayType>(
        				operand->getType()
        			)
        		) {

        			if (
        				ATy->getElementType() == Type::getFloatTy(G->getContext())
        			) {
        				unsigned numOfElements = ATy->getNumElements();

        				if (ConstantArray *arrayConst = dyn_cast<ConstantArray> (operand)) {
        					unsigned numOfElemOperands = arrayConst->getNumOperands();
        					for (unsigned i = 0; i < numOfElemOperands; i++) {
        						if (
        							ConstantFP* val = dyn_cast<ConstantFP>(
        								arrayConst->getOperand(i)
        							)
        						) {
        							double value = val->getValueAPF().convertToFloat();
        							Constant* newVal = ConstantFP::get(
        									Type::getDoubleTy(G->getContext()),
        									value
        							);
        							arrayConst->setOperand(i, newVal);
        						}
        						AbstractTypeUser::setType(
        							arrayConst->getOperand(i),
        							Type::getDoubleTy(G->getContext())
        						);
        					}
        				}
        				ArrayType *newArrayType = ArrayType::get(
        						Type::getDoubleTy(G->getContext()), numOfElements
        				);
        				AbstractTypeUser::setType(operand, newArrayType);
        			}
        		}
        	}
        }
        AbstractTypeUser::setType (
        		initializer, newSTy
        );
        AbstractTypeUser::setType (
        		G, PointerType::getUnqual(newSTy)
        );
    }


    // Handle array of constants
    void handleArrayGlobal(
    		GlobalVariable *G, const ArrayType *ATy
    ) {

        	Value *initializer = G->getInitializer();
        	if (
        		ATy->getElementType() == Type::getFloatTy(G->getContext())
        	) {
        		//FIXME: handle array of constants
        		unsigned numOfElements = ATy->getNumElements();

        		if (ConstantArray *arrayConst = dyn_cast<ConstantArray> (initializer)) {
        			unsigned numOfElemOperands = arrayConst->getNumOperands();
        			for (unsigned i = 0; i < numOfElemOperands; i++) {
        				Value *operand = arrayConst->getOperand(i);
        				if (operand->getType() == Type::getFloatTy(G->getContext())) {
        					if (
        						ConstantFP* val = dyn_cast<ConstantFP>(operand)
        					) {
        						double value = val->getValueAPF().convertToFloat();
        						Constant* newVal = ConstantFP::get(
        							Type::getDoubleTy(G->getContext()),
        							value
        						);
        						arrayConst->setOperand(i, newVal);
        					}
        					AbstractTypeUser::setType(
        						arrayConst->getOperand(i),
        						Type::getDoubleTy(G->getContext())
        					);
        				}
        			}
        		}
				ArrayType *newArrayType = ArrayType::get(
						Type::getDoubleTy(G->getContext()), numOfElements
				);
				AbstractTypeUser::setType(G, PointerType::getUnqual(newArrayType));
				AbstractTypeUser::setType(initializer, newArrayType);

        	} else if (
					const StructType *STy = dyn_cast<StructType> (
							ATy->getElementType()
					)
			) {
				StructType* newStructType = getNewType(STy);
				ArrayType *newArrayType = ArrayType::get(
						newStructType,
						ATy->getNumElements()
				);
				Type *newArrayPtrType = PointerType::getUnqual(
						newArrayType
				);

				if (ConstantArray *arrayConst = dyn_cast<ConstantArray> (initializer)) {
					unsigned numOfElements = arrayConst->getNumOperands();

					for (unsigned i = 0; i < numOfElements; i++) {
						Value *elem = arrayConst->getOperand(i);
						if (
								ConstantStruct *structConst = dyn_cast<ConstantStruct> (
										elem
								)
						) {
							unsigned numOfOps = structConst->getNumOperands();
							for (unsigned i = 0; i < numOfOps; i++) {
								Value *operand = structConst->getOperand(i);
								if (operand->getType() == Type::getFloatTy(G->getContext())) {
									if (
											ConstantFP* val = dyn_cast<ConstantFP>(
													operand
											)
									) {
										double value = val->getValueAPF().convertToFloat();
										Constant* newVal = ConstantFP::get(
												Type::getDoubleTy(G->getContext()),
												value
										);
										structConst->setOperand(i, newVal);
									}
									AbstractTypeUser::setType(
											operand, Type::getDoubleTy(G->getContext())
									);
								}
								else if (operand->getType() == Type::getFloatPtrTy(G->getContext()))
									AbstractTypeUser::setType(
											operand, Type::getDoublePtrTy(G->getContext())
									);
							}

						}
					}
				}
				AbstractTypeUser::setType(G, newArrayPtrType);
				AbstractTypeUser::setType(G->getInitializer(), newArrayType);
        	}
    }

    // return 1 if need to move to next instruction because of adding extend instructions
    int handleCall(Instruction *I) {
    	// Handle the function call
    	if (Function *function = dyn_cast<Function> (I->getOperand(0))) {
    		//errs() << function->getName() << "\n";
    		if (!function->isDeclaration()) {
    			replaceType(I);
    		}
    		// Handle external functions
    		else {
    			const FunctionType *functionType = function->getFunctionType();
    			unsigned argumentNum = functionType->getNumParams();
    			for (unsigned i = 0 ; i < argumentNum; i++) {
    				Value *param = I->getOperand(i+1);
    				const Type* paramType = functionType->getParamType(i);
    				// In this case, param type is double
    				if (paramType == Type::getFloatTy(I->getContext()) ) {
    					AbstractTypeUser::setType(
    							param, Type::getDoubleTy(I->getContext())
    					);
    					FPTruncInst *truncInst = new FPTruncInst(
    							param, Type::getFloatTy(I->getContext()),
    							"", I
    					);
    					I->setOperand( i+1, truncInst);
    				}
    				// In this case, the type of param is double ptr
    				else if (paramType == Type::getFloatPtrTy(I->getContext())) {
    					BitCastInst *bitcast = new BitCastInst (
    	    					param,
    	    					Type::getFloatPtrTy(I->getContext()), "", I
    					);
    					I->setOperand(i+1, bitcast);
    				} else if (
    						const StructType *STy = dyn_cast<StructType> (
    								paramType
    						)
    				) {

    					if (!isFloatStruct(STy))
    						continue;

    					const StructType *newSTy = getNewType(STy);
    					AllocaInst *newParam_addr = new AllocaInst (
    							STy, "new_param_addr", I
    					);

    					AbstractTypeUser::setType(param, newSTy);
    					AllocaInst *param_addr = new AllocaInst (
    							param->getType(), "param_addr", I
    					);
    					new StoreInst (param, param_addr, I);
    					unsigned numOfElements = STy->getNumElements();
    					for (unsigned j = 0; j < numOfElements; j++) {
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
    										param_addr, idx, idx+2, "", I
    								);
    						GetElementPtrInst *newelement =
    								GetElementPtrInst::CreateInBounds (
    										newParam_addr, idx, idx+2, "", I
    								);
    						if (newelement->getType() == element->getType()) {
    							LoadInst *load_elem = new LoadInst(element, "", I);
    							new StoreInst(load_elem, newelement, I);
    						}
    						else if (
    							STy->getElementType(j) == Type::getFloatTy(I->getContext())
    						) {
    							LoadInst *load_elem = new LoadInst(element, "", I);
    							FPTruncInst *truncInst = new FPTruncInst(
    									load_elem, Type::getFloatTy(I->getContext()),
    									"", I
    							);
    							new StoreInst(truncInst, newelement, I);
    						}
    					}
    					LoadInst *newParam = new LoadInst (newParam_addr, "", I);
    					I->setOperand(i+1, newParam);
    				}
    	    	} // end for
    	    	if (
    	    		functionType->getReturnType() == Type::getFloatTy(I->getContext())
    	    	) {
    	    		Instruction *next = I->getParent()->getInstList().getNext(I);
    	    		FPExtInst *extInst = new FPExtInst (
    	    				I->getUnderlyingObject(), Type::getDoubleTy(I->getContext()),
    	    				"", next
    	    		);
    	    		AbstractTypeUser::setType(I, Type::getDoubleTy(I->getContext()));
    	    		I->replaceAllUsesWith(extInst);
    	    		extInst->setOperand(0, I);
    	    		AbstractTypeUser::setType(I, Type::getFloatTy(I->getContext()));
    	    		return 1;
    	    	}
    	    }
    	}
    	//The weird case: bitcast call
    	else {
    		if (I->getType() == Type::getFloatTy(I->getContext())) {
    			Instruction *next = I->getParent()->getInstList().getNext(I);
    			FPExtInst *extInst = new FPExtInst (
    				I->getUnderlyingObject(), Type::getDoubleTy(I->getContext()),
    				"", next
    			);
    			AbstractTypeUser::setType(I, Type::getDoubleTy(I->getContext()));
    			I->replaceAllUsesWith(extInst);
    			extInst->setOperand(0, I);
    			AbstractTypeUser::setType(I, Type::getFloatTy(I->getContext()));
    			return 1;
    		}
    	}
    	return 0;
    }

    virtual bool changeFunctionPrototype(Function *F) {
    	// Change types of parameters
    	unsigned paramNum = F->getFunctionType()->getNumParams();

    	std::vector<const Type *> typeParams;

    	// Change the function signature
    	Function::arg_iterator argument = F->getArgumentList().begin();
    	Instruction *firstInst = F->getEntryBlock().getFirstNonPHI();
    	for (unsigned i = 0 ; i < paramNum; i++) {
    		Value *param = argument;
    		const Type* paramType = F->getFunctionType()->getParamType(i);
    		//HACK: Check and add integer type
    		if (paramType->isIntOrIntVectorTy())
    			typeParams.push_back(paramType);
    		else if (paramType == Type::getFloatTy(F->getContext()))
    			typeParams.push_back(Type::getDoubleTy(F->getContext()));
    		else if (
    			paramType == Type::getFloatPtrTy(F->getContext()) ||
    			paramType == Type::getDoublePtrTy(F->getContext())
    		) {
    			typeParams.push_back(Type::getDoubleTy(F->getContext()));

    			AllocaInst *alloc = new AllocaInst(
    					Type::getDoubleTy(F->getContext()), "", firstInst
    			);
    			AbstractTypeUser::setType(param, Type::getDoublePtrTy(F->getContext()));
    			param->replaceAllUsesWith(alloc);
    			AbstractTypeUser::setType(param, Type::getDoubleTy(F->getContext()));
    			new StoreInst(param, alloc, firstInst);
    		} else if (
    			const PointerType *ptrType = dyn_cast<PointerType> (paramType)
    		) {
    			if (const ArrayType *aty = dyn_cast<ArrayType> (ptrType->getElementType())){
    				if (
    						aty->getElementType() == Type::getFloatTy(F->getContext())
    				) {
    					ArrayType *newArrayType = ArrayType::get(
    							Type::getDoubleTy(F->getContext()),
    							aty->getNumElements()
    					);
    					Type *newArrayPtrType = PointerType::getUnqual(
    							newArrayType
    					);
    					AbstractTypeUser::setType(param, newArrayPtrType);
    					typeParams.push_back(newArrayPtrType);
    				}
    				else if (
    						const StructType *STy = dyn_cast<StructType> (
    								aty->getElementType()
    						)
    				) {
    					StructType* newStructType = getNewType(STy);
    					ArrayType *newArrayType = ArrayType::get(
    							newStructType,
    							aty->getNumElements()
    					);
    					Type *newArrayPtrType = PointerType::getUnqual(
    							newArrayType
    					);
    					AbstractTypeUser::setType(param, newArrayPtrType);
    					typeParams.push_back(newArrayPtrType);
    				}
    				else
    					typeParams.push_back(paramType);
    			}
    			else if (
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
    			} else
    				typeParams.push_back(paramType);
    		} else if (
    				const StructType *STy = dyn_cast<StructType> ( paramType )
    		) {
    			const StructType* newStructType = getNewType(STy);
    			typeParams.push_back(newStructType);
    			AbstractTypeUser::setType(param, newStructType);
    		}	else
    			typeParams.push_back(paramType);
    		argument++;
    	}
    	const Type *returnType = F->getFunctionType()->getReturnType();
    	if (returnType == Type::getFloatTy(F->getContext()))
    		returnType = Type::getDoubleTy(F->getContext());
    	FunctionType *newFunctionType = FunctionType::get(
    			returnType,
    			typeParams, F->isVarArg()
    	);
    	Type *newType = PointerType::getUnqual(newFunctionType);
    	AbstractTypeUser::setType(F, newType);

    	//Change type
    	for (Function::iterator BB = F->begin(), BE = F->end(); BB != BE; BB++) {
    		BasicBlock *B = BB;
    		for (BasicBlock::iterator II = B->begin(), IE = B->end(); II != IE; II++) {
    			Instruction *I = II;

    			if (I->getOpcode() == Instruction::FPExt ||
    					I->getOpcode() == Instruction::FPTrunc ){
    				II++;
    				AbstractTypeUser::setType(I, Type::getDoubleTy(F->getContext()));
    				AbstractTypeUser::setType(I->getOperand(0), Type::getDoubleTy(F->getContext()));
    				I->replaceAllUsesWith(I->getOperand(0));
    				I->eraseFromParent();
    				II--;
    				continue;
    			}
    			if (I->getOpcode() == Instruction::Call) {
    				int needToMove = handleCall(I);
    				if (needToMove == 1)
    					II++;
    			} else
    				replaceType(I);

    		}
    	}
    	return true;
    }
  };
}
char FortranByValue::ID = 0;
static RegisterPass<FortranByValue> X("FortranByValue", "Changing Fortran code to passing by value");
