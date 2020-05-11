//===----------------------------------------------------------------------===//
//
// This file implements the transformation methods for making
//floating point exceptions explicit
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
  struct Ariadne: public FunctionPass{
	  static char ID; // Pass identification, replacement for typeid
      //Standard functions
	  Function *dbl_fabs;
	  Function *flt_fabs;
      Function *printf;
      Function *exit;
      Function *klee_make_symbolic;

      //Add capture expression function before every transformation of
      // special function
      Function *captureExpr;
      //Add raiseException to detect specific type of exception
      Function *raiseException;
      //Set the location that exceptions can occur
      Function *setExceptionLocation;

      // FIXME: at this time, we do not know how to get switches from opt
      // We handle the switch with normal or ariadne mode by setting and getting
      // __ARIADNE_NORMAL value
      bool isConcrete;

      //Constant list
      Constant *dbl_max;
      Constant *dbl_min;
      Constant *zero_int;
      Constant *zero_dbl;
      Constant *one_dbl;
      Constant *log_DBLMAX;
      Constant *log_DBLMIN;
      Constant *exp_nDBLMAX;
      Constant *exp_DBLMIN;

      Constant *flt_max;
      Constant *flt_min;
      Constant *zero_flt;
      Constant *one_flt;
      Constant *log_FLTMAX;
      Constant *log_FLTMIN;
      Constant *exp_nFLTMAX;
      Constant *exp_FLTMIN;
      Constant *int_max;

      Constant *FE_OVERFLOW; // 0
      Constant *FE_UNDERFLOW; // 1
      Constant *FE_INVALID; // 2
      Constant *FE_DIVBYZERO; // 3

      GlobalVariable *__ariadne_tmp;
      GlobalVariable *__sqrt_tmp;
      GlobalVariable *__log_tmp;
      GlobalVariable *__exp_tmp;
      GlobalVariable *__sin_tmp;
      GlobalVariable *__cos_tmp;
      GlobalVariable *__pow_tmp;
      GlobalVariable *infeasible_Path;

      //Static counters for exceptions
      int numOfOverflow;
      int numOfUnderflow;
      int numOfDivisionByZero;
      int numOfInvalid;

	  Ariadne() : FunctionPass(&ID) { }

      AssemblyAnnotationWriter w;

      //Print out an error message at the end of a basic block
      void printErrMsg(
    		  GlobalVariable *msg, BasicBlock *bb, bool withUnreach=true
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

    	  CallInst *print_call = CallInst::Create(printf,
				    params, array_endof(params),
    	      			  "", bb
    	  );
    	  print_call->setTailCall(true);

    	  Value *exit_params[] = {
    			  zero_32
    	  };

    	  CallInst *exit_call = CallInst::Create(exit,
    			  exit_params, array_endof(exit_params),"", bb);
    	  if (withUnreach){
    		  UnreachableInst *unreach =
    				  new UnreachableInst(bb->getContext(),bb);
    		  exit_call->moveBefore(unreach);
    	  }
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

      Function *getFabs(const Type *ty, Module *module) {
      		if (ty == Type::getFloatTy(module->getContext()) ) {
      			return flt_fabs;
      		} else if ( ty == Type::getDoubleTy(module->getContext())) {
      			return dbl_fabs;
      		} else {
      			Function *fabs = cast<Function>(module->getOrInsertFunction(
      					"__ariadne_fabs", ty, ty, NULL)
      			);
      			return fabs;
      		}
      }


      CallInst* fabs_call (Value *param, BasicBlock* bb){
			Module *module = bb->getParent()->getParent();
			Function* fabs = getFabs(param->getType(), module);
			Value *fabs_params[] = {
              	 param
			};

			CallInst *fabs_call = CallInst::Create(
					fabs, fabs_params, array_endof(fabs_params),
          			  "", bb
			);
			fabs_call->setTailCall(true);
			return fabs_call;
      }

      /*
       * To name the variables, the prefix x represents the first operator
       * the prefix y represents the second operator
       */

      //Process Add, Subtract operations
      void processOp (
    	  Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock
      ) {
    	  Value *min = getMin(I->getType(), I->getContext());
    	  Value *max = getMax(I->getType(), I->getContext());
    	  Value *zeroFP = getZeroFP(I->getType(), I->getContext());
    	  // Get the reference of the instruction
    	  Value *res = I->getUnderlyingObject();
    	  // Move the instruction to previous basic block
    	  I->removeFromParent();
    	  preBlock->getInstList().push_back(I);

    	  CallInst *fabsInst = fabs_call(res,preBlock);

    	  // Compare with max
    	  CmpInst *ogtMaxOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_OGT,
    			  fabsInst->getUnderlyingObject(), max, "", preBlock
    	  );

    	  BasicBlock *overFlowBB = BasicBlock::Create(
    			  preBlock->getContext(), "", preBlock->getParent()
    	  );
    	  overFlowBB->moveAfter(preBlock);
    	  // Comparing min basic block
    	  BasicBlock *minCmpBB = BasicBlock::Create(
    			  preBlock->getContext(), "", preBlock->getParent()
    	  );
    	  minCmpBB->moveAfter(overFlowBB);

    	  BranchInst *brOverFlowInst = BranchInst::Create(
    			  overFlowBB, minCmpBB,
    			  ogtMaxOp->getUnderlyingObject(), preBlock
    	  );
    	  ogtMaxOp->moveBefore(brOverFlowInst);

    	  //Overflow Exception Case
    	  //Declare global message for types of exceptions
    	  numOfOverflow ++;
    	  std::stringstream overFlowStream;
    	  overFlowStream << "Potential Overflow " << numOfOverflow << " \n";
    	  Constant *overFlowMsg = ConstantArray::get(
    	  		  I->getContext(), overFlowStream.str().c_str(), true
    	  );

    	  GlobalVariable *overFlowString = new GlobalVariable(
    	          *I->getParent()->getParent()->getParent(),
    	          overFlowMsg->getType(),
    	          true,
    	          GlobalValue::InternalLinkage,
    	          overFlowMsg,
    	          "overflow"
    	  );
    	  if (!isConcrete) {
    		  Constant *location = ConstantInt::get(
    				  Type::getInt32Ty(I->getContext()), numOfOverflow
    		  );
    		  setLocationCall(location, overFlowBB);

    		  raiseExceptionCall(FE_OVERFLOW, overFlowBB);
    	  }
    	  printErrMsg(overFlowString, overFlowBB);

    	  // Compare result with Zero
    	  CmpInst *ogtZeroOp = CmpInst::Create(
    			  Instruction::FCmp,
    			  FCmpInst::FCMP_OGT, fabsInst->getUnderlyingObject(),
    			  zeroFP,"", minCmpBB
    	  );

    	  // Compare with min
    	  CmpInst *ogtMinOp = CmpInst::Create(
    			  Instruction::FCmp,
    			  FCmpInst::FCMP_OLT, fabsInst->getUnderlyingObject(),
    			  min,"", minCmpBB
    	  );

    	  BinaryOperator *underFlowCond = BinaryOperator::Create(
    			  Instruction::And,
    			  ogtZeroOp->getUnderlyingObject(),
    			  ogtMinOp->getUnderlyingObject(), "", minCmpBB
    	  );

    	  BasicBlock *underFlowBB = BasicBlock::Create(
    			  preBlock->getContext(),"",
    			  preBlock->getParent()
    	  );

    	  underFlowBB->moveAfter(minCmpBB);
    	  underFlowBB->moveBefore(postBlock);

    	  BranchInst *brUnderFlowInst = BranchInst::Create(
    			  underFlowBB, postBlock,
    			  underFlowCond->getUnderlyingObject(),minCmpBB
    	  );

    	  underFlowCond->moveBefore(brUnderFlowInst);

    	  /// Underflow Exception Case
    	  /// Declare global message for types of exceptions
    	  numOfUnderflow ++;
    	  std::stringstream underFlowStream;
    	  underFlowStream << "Potential Underflow " << numOfUnderflow << " \n";
    	  Constant *underFlowMsg = ConstantArray::get(
    	  	  I->getContext(), underFlowStream.str().c_str(), true
    	  );

    	  GlobalVariable *underFlowString = new GlobalVariable(
    		  *I->getParent()->getParent()->getParent(),
    		  underFlowMsg->getType(),
    		  true,
    		  GlobalValue::InternalLinkage,
    		  underFlowMsg,
    		  "underflow"
    	  );
    	  if (!isConcrete) {
    		  Constant *location = ConstantInt::get(
			  Type::getInt32Ty(I->getContext()), numOfUnderflow
    		  );
    		  setLocationCall(location, underFlowBB);
    		  raiseExceptionCall(FE_UNDERFLOW, underFlowBB);
    	  }
    	  printErrMsg(underFlowString,underFlowBB);
      }

      void processMultOp (
		Instruction *I, BasicBlock *preBlock,
			BasicBlock *postBlock
      ) {
    	Value *zeroFP = getZeroFP(I->getType(), I->getContext());
    	Value *operand1 = I->getOperand(0);
    	Value *operand2 = I->getOperand(1);
    	Value *min = getMin(I->getType(), I->getContext());
		Value *max = getMax(I->getType(), I->getContext());
		// Get the reference of the instruction
		Value *res = I->getUnderlyingObject();
		// Move the instruction to previous basic block
		I->removeFromParent();
		preBlock->getInstList().push_back(I);

		CallInst *fabsInst = fabs_call(res, preBlock);

		// Compare with max
		CmpInst *ogtMaxOp = CmpInst::Create(Instruction::FCmp,
				FCmpInst::FCMP_OGT, fabsInst->getUnderlyingObject(), max, "",
				preBlock);

		BasicBlock *overFlowBB = BasicBlock::Create(preBlock->getContext(), "",
				preBlock->getParent());
		overFlowBB->moveAfter(preBlock);
		// Comparing min basic block
		BasicBlock *minCmpBB = BasicBlock::Create(preBlock->getContext(), "",
				preBlock->getParent());
		minCmpBB->moveAfter(overFlowBB);

		BranchInst *brOverFlowInst = BranchInst::Create(overFlowBB, minCmpBB,
				ogtMaxOp->getUnderlyingObject(), preBlock);
		ogtMaxOp->moveBefore(brOverFlowInst);

		//Overflow Exception Case
		//Declare global message for types of exceptions
		numOfOverflow++;
		std::stringstream overFlowStream;
		overFlowStream << "Potential Overflow " << numOfOverflow << " \n";
		Constant *overFlowMsg = ConstantArray::get(I->getContext(),
				overFlowStream.str().c_str(), true);

		GlobalVariable *overFlowString = new GlobalVariable(
				*I->getParent()->getParent()->getParent(),
				overFlowMsg->getType(), true, GlobalValue::InternalLinkage,
				overFlowMsg, "overflow");
		printErrMsg(overFlowString, overFlowBB);

		// Compare operands with zeros
		CmpInst *Op1neZero = CmpInst::Create(Instruction::FCmp,
				FCmpInst::FCMP_ONE, operand1, zeroFP, "", minCmpBB);
		CmpInst *Op2neZero = CmpInst::Create(Instruction::FCmp,
				FCmpInst::FCMP_ONE, operand2, zeroFP, "", minCmpBB);
		// Compare result with min
		CmpInst *oltMinOp = CmpInst::Create(Instruction::FCmp,
				FCmpInst::FCMP_OLT, fabsInst->getUnderlyingObject(), min, "",
				minCmpBB);
		/// Condition for Underflow
		BinaryOperator *andCond = BinaryOperator::Create(
			  Instruction::And, Op1neZero->getUnderlyingObject(),
			  Op2neZero->getUnderlyingObject(), "", minCmpBB
		);
		BinaryOperator *underflowCond = BinaryOperator::Create(
			  Instruction::And, andCond->getUnderlyingObject(),
			  oltMinOp->getUnderlyingObject(), "", minCmpBB
		);

		BasicBlock *underFlowBB = BasicBlock::Create(preBlock->getContext(),
				"", preBlock->getParent());

		underFlowBB->moveAfter(minCmpBB);
		underFlowBB->moveBefore(postBlock);

		BranchInst::Create(
				underFlowBB, postBlock,
				underflowCond->getUnderlyingObject(), minCmpBB
		);

		/// Underflow Exception Case
		/// Declare global message for types of exceptions
		numOfUnderflow++;
		std::stringstream underFlowStream;
		underFlowStream << "Potential Underflow " << numOfUnderflow << " \n";
		Constant *underFlowMsg = ConstantArray::get(I->getContext(),
				underFlowStream.str().c_str(), true);

		GlobalVariable *underFlowString = new GlobalVariable(
				*I->getParent()->getParent()->getParent(),
				underFlowMsg->getType(), true, GlobalValue::InternalLinkage,
				underFlowMsg, "underflow");
		printErrMsg(underFlowString, underFlowBB);
      }

      void processDivOp (
    	  Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock
      ) {
    	  Value *min = getMin(I->getType(), I->getContext());
    	  Value *max = getMax(I->getType(), I->getContext());
    	  Value *zeroFP = getZeroFP(I->getType(), I->getContext());
    	  Value *operand1 = I->getOperand(0);
    	  Value *operand2 = I->getOperand(1);
    	  // Compare y with zero
    	  CmpInst *y_eqZeroOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_OEQ,
    			  operand2, zeroFP, "", preBlock
    	  );

    	  //Compare the first operand with zero
    	  BasicBlock *xCmpZeroBB = BasicBlock::Create(
    			  preBlock->getContext(),"", preBlock->getParent()
    	  );
    	  xCmpZeroBB->moveAfter(preBlock);
    	  // y > max * x ?
    	  BasicBlock *cmpMaxBB = BasicBlock::Create(
    			  preBlock->getContext(),"", preBlock->getParent()
    	  );

    	  cmpMaxBB->moveAfter(xCmpZeroBB);

    	  BranchInst::Create(
    			  xCmpZeroBB,cmpMaxBB, y_eqZeroOp->getUnderlyingObject(), preBlock
    	  );

    	  CmpInst *x_eqZeroOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_OEQ,
    			  operand1, zeroFP, "", xCmpZeroBB
    	  );

    	  BasicBlock *invalidBB = BasicBlock::Create(
    			  preBlock->getContext(),"",
    			  preBlock->getParent()
    	  );

    	  invalidBB->moveAfter(xCmpZeroBB);
    	  BasicBlock *divideByZeroBB = BasicBlock::Create(
    			  preBlock->getContext(),"", preBlock->getParent()
    	  );
    	  divideByZeroBB->moveAfter(invalidBB);

    	  BranchInst::Create(
    			  invalidBB, divideByZeroBB,
    			  x_eqZeroOp->getUnderlyingObject(), xCmpZeroBB
    	  );


    	  //Invalid Exception Case
    	  //Declare global message for types of exceptions
    	  numOfInvalid ++;
    	  std::stringstream invalidStream;
    	  invalidStream << "Potential Invalid " << numOfInvalid << " \n";
    	  Constant *invalidMsg = ConstantArray::get(
    			  I->getContext(), invalidStream.str().c_str(), true
    	  );

    	  GlobalVariable *invalidString = new GlobalVariable(
    			  *I->getParent()->getParent()->getParent(),
    			  invalidMsg->getType(),
    			  true,
    			  GlobalValue::InternalLinkage,
    			  invalidMsg,
    			  "invalid"
    	  );
	  if (!isConcrete) {
    		  Constant *location = ConstantInt::get(
    				Type::getInt32Ty(I->getContext()), numOfInvalid
    		  );
    		  setLocationCall(location, invalidBB);
    		  raiseExceptionCall(FE_INVALID, invalidBB);

    	  }
    	  printErrMsg(invalidString,invalidBB);

    	  //Divide By Zero Exception Case
    	  //Declare global message for types of exceptions
    	  numOfDivisionByZero ++;
    	  std::stringstream divideByZeroStream;
    	  divideByZeroStream << "Potential Division By Zero " << numOfDivisionByZero << " \n";
    	  Constant *divideByZeroMsg = ConstantArray::get(
    			  I->getContext(), divideByZeroStream.str().c_str(), true
    	  );

    	  GlobalVariable *divideByZeroString = new GlobalVariable(
    			  *I->getParent()->getParent()->getParent(),
    			  divideByZeroMsg->getType(),
    			  true,
    			  GlobalValue::InternalLinkage,
    			  divideByZeroMsg,
    			  "divisionbyZero"
    	  );
    	  if (!isConcrete) {
    		  Constant *location = ConstantInt::get(
    				  Type::getInt32Ty(I->getContext()), numOfDivisionByZero
    		  );
    		  setLocationCall(location, divideByZeroBB);
    		  raiseExceptionCall(FE_DIVBYZERO, divideByZeroBB);
    	  }
    	  printErrMsg(divideByZeroString,divideByZeroBB);

    	  CallInst *x_fabs_call = fabs_call(operand1,cmpMaxBB);
    	  CallInst *y_fabs_call = fabs_call(operand2,cmpMaxBB);

    	  BinaryOperator *mulMaxOp = BinaryOperator::Create(
    			  Instruction::FMul, y_fabs_call->getUnderlyingObject(),max
    	  );

    	  cmpMaxBB->getInstList().push_back(mulMaxOp);

    	  CmpInst *oltOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_OLT,
    			  mulMaxOp->getUnderlyingObject(),
    			  x_fabs_call->getUnderlyingObject(),"", cmpMaxBB
    	  );

    	  BasicBlock *overFlowBB = BasicBlock::Create(
    			  preBlock->getContext(),"", preBlock->getParent()
    	  );

    	  overFlowBB->moveAfter(cmpMaxBB);
    	  BasicBlock *x_absGtZeroBB = BasicBlock::Create(
    			  preBlock->getContext(),"", preBlock->getParent()
    	  );
    	  x_absGtZeroBB->moveAfter(overFlowBB);
    	  BranchInst::Create(
    			  overFlowBB, x_absGtZeroBB,
    			  oltOp->getUnderlyingObject(), cmpMaxBB
    	  );

    	  //Overflow Exception Case
    	  //Declare global message for types of exceptions
    	  numOfOverflow ++;
    	  std::stringstream overFlowStream;
    	  overFlowStream << "Potential Overflow " << numOfOverflow << " \n";
    	  Constant *overFlowMsg = ConstantArray::get(
    			  I->getContext(), overFlowStream.str().c_str(), true
    	  );

    	  GlobalVariable *overFlowString = new GlobalVariable(
    			  *I->getParent()->getParent()->getParent(),
    			  overFlowMsg->getType(),
    			  true,
    			  GlobalValue::InternalLinkage,
    			  overFlowMsg,
    			  "overflow"
    	  );
    	  if (!isConcrete) {
    		  Constant *location = ConstantInt::get(
    				  Type::getInt32Ty(I->getContext()), numOfOverflow
    		  );
    		  setLocationCall(location, overFlowBB);
    		  raiseExceptionCall(FE_OVERFLOW, overFlowBB);

    	  }
    	  printErrMsg(overFlowString,overFlowBB);

    	  CmpInst *ogtZeroOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_OGT,
    			  x_fabs_call->getUnderlyingObject(),
    			  zeroFP,"", x_absGtZeroBB
    	  );

    	  BasicBlock *cmpMinBB = BasicBlock::Create(
    			  preBlock->getContext(),"", preBlock->getParent()
    	  );
    	  cmpMinBB->moveAfter(x_absGtZeroBB);
    	  BranchInst *ogtZeroBr = BranchInst::Create(
    			  cmpMinBB,postBlock,
    			  ogtZeroOp->getUnderlyingObject(),x_absGtZeroBB
    	  );
    	  ogtZeroOp->moveBefore(ogtZeroBr);

    	  BinaryOperator *mulMinOp = BinaryOperator::Create(
    			  Instruction::FMul, y_fabs_call->getUnderlyingObject(),min
    	  );

    	  cmpMinBB->getInstList().push_back(mulMinOp);
    	  CmpInst *ogtMinOp = CmpInst::Create(
    			  Instruction::FCmp,
    			  FCmpInst::FCMP_OGT, mulMinOp->getUnderlyingObject(),
    			  x_fabs_call->getUnderlyingObject(),"", cmpMinBB
    	  );


    	  BasicBlock *underFlowBB = BasicBlock::Create(
    			  preBlock->getContext(),"", preBlock->getParent()
    	  );

    	  underFlowBB->moveAfter(cmpMinBB);
    	  BranchInst::Create(
    			  underFlowBB,postBlock,
    			  ogtMinOp->getUnderlyingObject(), cmpMinBB
    	  );

    	  //Underflow Exception Case
    	  //Declare global message for types of exceptions
    	  numOfUnderflow ++;
    	  std::stringstream underFlowStream;
    	  underFlowStream << "Potential Underflow " << numOfUnderflow << " \n";
    	  Constant *underFlowMsg = ConstantArray::get(
    			  I->getContext(), underFlowStream.str().c_str(), true
    	  );

    	  GlobalVariable *underFlowString = new GlobalVariable(
    			  *I->getParent()->getParent()->getParent(),
    			  underFlowMsg->getType(),
    			  true,
    			  GlobalValue::InternalLinkage,
    			  underFlowMsg,
    			  "underflow"
    	  );
    	  if (!isConcrete) {
    		  Constant *location = ConstantInt::get(
    				  Type::getInt32Ty(I->getContext()), numOfUnderflow
    		  );
    		  setLocationCall(location, underFlowBB);
    		  raiseExceptionCall(FE_UNDERFLOW, underFlowBB);
    	  }
    	  printErrMsg(underFlowString, underFlowBB);
    	  I->removeFromParent();
     	  postBlock->getInstList().push_back(I);
     	  postBlock->moveAfter(underFlowBB);

      }

      /* HANDLE SPECIAL FUNCTIONS */

      /* Call klee_make_symbolic_function
       * FIXME: This time, this function take a default name sqrt_tmp.
       * This should be replaced by an appropriate names after
      */
      LoadInst* klee_call(Constant *msg, BasicBlock *bb, const Type *type){

    	  Constant *zero_32 = Constant::getNullValue(
    			  IntegerType::getInt32Ty(bb->getContext())
    	  );

    	  Constant *gep_params[] = {
    			  zero_32,
    			  zero_32
    	  };

    	  //Create a new fresh symbolic variable
    	  AllocaInst *alloc_symbolic = new AllocaInst(
    			  type,"",bb
    	  );

    	  BitCastInst *klee_var = new BitCastInst(
    			  alloc_symbolic,
    			  Type::getInt8PtrTy(bb->getContext()),"",bb
		  );

    	  Constant *klee_tmp_var = ConstantExpr::getGetElementPtr(
    			  msg, gep_params,
    			  array_lengthof(gep_params)
    	  );

    	  int numOfBytes = 8;
    	  if (type == Type::getFloatTy(bb->getContext()))
    		  numOfBytes = 4;
    	  Value *klee_params[] = {
    			  klee_var,
    			  ConstantInt::get(bb->getContext(), APInt(32, numOfBytes)),
    			  klee_tmp_var
    	  };
    	  CallInst::Create(
    			  klee_make_symbolic,
    			  klee_params, array_endof(klee_params),
    			  "", bb
    	  );

    	  LoadInst *load_symbolic = new LoadInst(
    			  alloc_symbolic->getUnderlyingObject(),"",bb
    	  );
    	  //load_symbolic->moveBefore(klee_call);
    	  return load_symbolic;
      }

      // The function captures the symbolic variables

      CallInst *captureExprCall(Value *value, BasicBlock *bb ) {
    	  Value *params[] = {
    			  value
    	  };

    	  CallInst *call = CallInst::Create (
    			  captureExpr,
    			  params, array_endof(params),
    			  "", bb
    	  );
    	  return call;
      }

      CallInst *raiseExceptionCall(Value *value, BasicBlock *bb) {
    	  Value *params[] = {
    			  value
    	  };

    	  CallInst *call = CallInst::Create (
    			  raiseException,
    			  params, array_endof(params),
    			  "", bb
    	  );
    	  return call;

      }

      CallInst *setLocationCall(Value *value, BasicBlock *bb) {
          	  Value *params[] = {
          			  value
          	  };

          	  CallInst *call = CallInst::Create (
          			  setExceptionLocation,
          			  params, array_endof(params),
          			  "", bb
          	  );
          	  return call;
      }

      /* Rewrite sqrt function in new semantics
       * Naming: param prefix represents x, res prefix represents sqrt(x)
       * Note: sqrt does not overflow/underflow, nor has division by zero.
       * sqrt(x) =>
       *  1. if x < 0, we have an invalid
       *  2. if x >= 0, generate fresh symbolic variable y to denote sqrt(x)
       *    call captureExpr function to get symbolic expression of x.
       */
      void handleSqrt(
    		  Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock
      ) {
    	  // Get the parameter
    	  Value* param = I->getOperand(1);
    	  Value* zeroFP = getZeroFP(I->getType(), I->getContext());

    	  CmpInst *paramOltZeroOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_OLT, param,
    			  zeroFP,"", preBlock
    	  );

    	  BasicBlock *invalidBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );

    	  invalidBB->moveAfter(preBlock);

    	  BranchInst::Create(
    			  invalidBB, postBlock,
    			  paramOltZeroOp->getUnderlyingObject(), preBlock
    	  );

    	  //Invalid Exception Case
    	  //Declare global message for types of exceptions
    	  numOfInvalid ++;
    	  std::stringstream invalidStream;
    	  invalidStream << "Potential Invalid " << numOfInvalid << "d \n";
    	  Constant *invalidMsg = ConstantArray::get (
    			  I->getContext(), invalidStream.str().c_str(), true
    	  );

    	  GlobalVariable *invalidString = new GlobalVariable(
    			  *I->getParent()->getParent()->getParent(),
    			  invalidMsg->getType(),
    			  true,
    			  GlobalValue::InternalLinkage,
    			  invalidMsg,
    			  "invalid"
    	  );
    	  if (!isConcrete) {
    		  Constant *location = ConstantInt::get(
    				  Type::getInt32Ty(I->getContext()), numOfInvalid
    		  );
    		  setLocationCall(location, invalidBB);
    		  raiseExceptionCall(FE_INVALID, invalidBB);
    	  }
    	  printErrMsg(invalidString,invalidBB);

    	  //Only create new symbolic variable when not in normal mode
    	  if (!isConcrete) {
    		  captureExprCall(param,postBlock);

			  LoadInst *klee_symbolic = klee_call(
      			  __sqrt_tmp, postBlock, I->getType()
			  );

			  I->replaceAllUsesWith(klee_symbolic->getUnderlyingObject());
			  I->eraseFromParent();
    	  } else {
    		  I->removeFromParent();
    		  postBlock->getInstList().push_back(I);
    	  }

      }

      /*
       * Rewrite pow function
       *  pow(x,y) =>
       *  1. if (x <= 0) && (y <= 0), we have invalid
       *  2. otherwise, generate fresh symbolic variable z to denote pow(x,y)
       *    a. x = 0 && z = 0
       *    b. 0 < x <= 1 && 0 < z <= 1
       *    c. x > 1 && z > 1
       */

      void handlePow (
    		  Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock
      ) {
    	  Value* oneFP = getOneFP(I->getType(), I->getContext());
    	  Value* zeroFP = getZeroFP(I->getType(), I->getContext());
    	  Value *param1 = I->getOperand(1);
    	  Value *param2 = I->getOperand(2);
    	  CmpInst *param1GtZeroOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_UGT,
    			  param1, zeroFP, "", preBlock
    	  );
    	  CmpInst *param2GtZeroOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_UGT,
    			  param2, zeroFP, "", preBlock
    	  );

    	  BinaryOperator *orCond1 = BinaryOperator::Create(
    			  Instruction::Or, param1GtZeroOp->getUnderlyingObject(),
    			  param2GtZeroOp->getUnderlyingObject(),"",preBlock
    	  );

    	  BasicBlock *invalidBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  invalidBB->moveAfter(preBlock);
    	  BasicBlock *param1EqZeroBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  param1EqZeroBB->moveAfter(invalidBB);
    	  BranchInst::Create(
    			  param1EqZeroBB,invalidBB,
    			  orCond1->getUnderlyingObject(), preBlock
    	  );
    	  //Declare global message for types of exceptions
    	  // Invalid Exceptions
    	  numOfInvalid ++;
    	  std::stringstream invalidStream;
    	  invalidStream << "Potential Invalid " << numOfInvalid << "d \n";
    	  Constant *invalidMsg = ConstantArray::get(
    			  I->getContext(), invalidStream.str().c_str(), true
    	  );

    	  GlobalVariable *invalidString = new GlobalVariable(
    			  *I->getParent()->getParent()->getParent(),
    			  invalidMsg->getType(),
    			  true,
    			  GlobalValue::InternalLinkage,
    			  invalidMsg,
    			  "invalid"
    	  );
    	  if (!isConcrete) {
    		  Constant *location = ConstantInt::get(
    				  Type::getInt32Ty(I->getContext()), numOfInvalid
    		  );
    		  setLocationCall(location, invalidBB);
    		  raiseExceptionCall(FE_INVALID, invalidBB);
    	  }
    	  printErrMsg(invalidString,invalidBB);
    	  LoadInst *klee_symbolic = NULL;
    	  if (!isConcrete) {
    		  klee_symbolic = klee_call(
    			  __pow_tmp,param1EqZeroBB, I->getType()
    		  );
    	  } else {
    		  I->removeFromParent();
    		  param1EqZeroBB->getInstList().push_back(I);
    	  }

    	  CmpInst *param1EqZeroOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_OEQ,
    			  param1, zeroFP, "", param1EqZeroBB
    	  );

    	  BasicBlock *resEqZeroBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  resEqZeroBB->moveAfter(param1EqZeroBB);
    	  BasicBlock *orCondBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  orCondBB->moveAfter(resEqZeroBB);
    	  BranchInst::Create(
    			  resEqZeroBB, orCondBB,
    			  param1EqZeroOp->getUnderlyingObject(), param1EqZeroBB
    	  );
    	  CmpInst *resEqZeroOp;
    	  if (!isConcrete)
    		  resEqZeroOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_OEQ,
    			  klee_symbolic->getUnderlyingObject(),
    			  zeroFP, "", resEqZeroBB
    		  );
    	  else
    		  resEqZeroOp = CmpInst::Create(
    				  Instruction::FCmp, FCmpInst::FCMP_OEQ,
    				  I->getUnderlyingObject(),
    				  zeroFP, "", resEqZeroBB
    		  );

    	  BranchInst::Create(
    			  postBlock, orCondBB,
    			  resEqZeroOp->getUnderlyingObject(), resEqZeroBB
    	  );

    	  CmpInst *param1LeZeroOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_ULE,
    			  param1, zeroFP, "", orCondBB
    	  );
    	  CmpInst *param1GtOneOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_UGT,
    			  param1, oneFP, "", orCondBB
    	  );

    	  BinaryOperator *orCond2 = BinaryOperator::Create(
    			  Instruction::Or, param1LeZeroOp->getUnderlyingObject(),
    			  param1GtOneOp->getUnderlyingObject(),"", orCondBB
    	  );

    	  BasicBlock *resLeZeroBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  resLeZeroBB->moveAfter(orCondBB);
    	  BasicBlock *param1LeOneBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  param1LeOneBB->moveAfter(resLeZeroBB);
    	  BranchInst::Create(
    			  param1LeOneBB, resLeZeroBB,
    			  orCond2->getUnderlyingObject(), orCondBB
    	  );

    	  CmpInst *resLeZeroOp;
    	  if (!isConcrete)
    		  resLeZeroOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_ULE,
    			  klee_symbolic->getUnderlyingObject(),
    			  zeroFP, "", resLeZeroBB
    		  );
    	  else
    		  resLeZeroOp = CmpInst::Create(
    				  Instruction::FCmp, FCmpInst::FCMP_ULE,
    				  I->getUnderlyingObject(),
    				  zeroFP, "", resLeZeroBB
    		  );
    	  BasicBlock *resGtOneBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  resGtOneBB->moveAfter(param1LeOneBB);
    	  BranchInst::Create(
    			  param1LeOneBB, resGtOneBB,
    			  resLeZeroOp->getUnderlyingObject(), resLeZeroBB
    	  );
    	  CmpInst *resGtOneOp;
    	  if (!isConcrete)
    		  resGtOneOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_UGT,
    			  klee_symbolic->getUnderlyingObject(), oneFP, "", resGtOneBB
    		  );
    	  else
    		  resGtOneOp = CmpInst::Create(
    				  Instruction::FCmp, FCmpInst::FCMP_UGT,
    				  I->getUnderlyingObject(), oneFP, "", resGtOneBB
    		  );
    	  BranchInst::Create(
    			  param1LeOneBB,postBlock,
    			  resGtOneOp->getUnderlyingObject(), resGtOneBB
    	  );

    	  CmpInst *param1LeOneOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_ULE,
    			  param1, oneFP, "", param1LeOneBB
    	  );

    	  BasicBlock *resLeOneBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  resLeOneBB->moveAfter(param1LeOneBB);
    	  BasicBlock *unreachableBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  unreachableBB->moveAfter(resLeOneBB);
    	  BranchInst::Create(
    			  unreachableBB, resLeOneBB,
    			  param1LeOneOp->getUnderlyingObject(), param1LeOneBB
    	  );

    	  CmpInst *resLeOneOp;
    	  if (!isConcrete)
    		  resLeOneOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_ULE,
    			  klee_symbolic->getUnderlyingObject(), oneFP, "", resLeOneBB
    		  );
    	  else
    		  resLeOneOp = CmpInst::Create(
    				  Instruction::FCmp, FCmpInst::FCMP_ULE,
    				  I->getUnderlyingObject(), oneFP, "", resLeOneBB
    		  );
    	  BranchInst::Create(
    			  unreachableBB, postBlock,
    			  resLeOneOp->getUnderlyingObject(), resLeOneBB
    	  );

    	  new UnreachableInst(
    			  preBlock->getContext(),unreachableBB
    	  );

    	  if (!isConcrete) {
    		  I->replaceAllUsesWith(klee_symbolic->getUnderlyingObject());
    		  I->eraseFromParent();
    	  }

      }

      /* Rewrite exp into new Semantics
       * Note: exp (base e) can overflow & underflow, but no invalid or
	   * division by zero
	   * exp(x) =>
       * 1. x > log(max) => overflow
	   * 2. x < log(min) => underflow
       * 3. generate fresh symbolic variable y to denote exp(x)
       *    call captureExpr function to get symbolic expression of x.
       *
       */
      void handleExp (
    		  Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock
      ) {
    	  Value* logMaxFP = getLogMax(I->getType(), I->getContext());
    	  Value* logMinFP = getLogMin(I->getType(), I->getContext());
    	  Value *param = I->getOperand(1);
    	  CmpInst *logMaxLtParamOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_OLT, logMaxFP,
    			  param, "", preBlock
    	  );

    	  BasicBlock *overFlowBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  overFlowBB->moveAfter(preBlock);
    	  BasicBlock *logMinGtParamBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  logMinGtParamBB->moveAfter(overFlowBB);
    	  BranchInst::Create(
    			  overFlowBB, logMinGtParamBB,
    			  logMaxLtParamOp->getUnderlyingObject(), preBlock
    	  );
    	  //Overflow Exception
    	  //Declare global message for types of exceptions
    	  numOfOverflow ++;
    	  std::stringstream overFlowStream;
    	  overFlowStream << "Potential Overflow " << numOfOverflow << "d \n";
    	  Constant *overFlowMsg = ConstantArray::get(
    			  I->getContext(), overFlowStream.str().c_str(), true
    	  );

    	  GlobalVariable *overFlowString = new GlobalVariable(
    			  *I->getParent()->getParent()->getParent(),
    			  overFlowMsg->getType(),
    			  true,
    			  GlobalValue::InternalLinkage,
    			  overFlowMsg,
    			  "overflow"
    	  );
    	  if (!isConcrete) {
    		  Constant *location = ConstantInt::get(
    				  Type::getInt32Ty(I->getContext()), numOfOverflow
    		  );
    		  setLocationCall(location, overFlowBB);
    		  raiseExceptionCall(FE_OVERFLOW, overFlowBB);
    	  }
    	  printErrMsg(overFlowString,overFlowBB);

    	  CmpInst *logMinGtParamOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_OGT,
    			  logMinFP, param, "", logMinGtParamBB
    	  );
    	  BasicBlock *underFlowBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  underFlowBB->moveAfter(logMinGtParamBB);

    	  BranchInst::Create(
    			  underFlowBB, postBlock,
    			  logMinGtParamOp->getUnderlyingObject(), logMinGtParamBB
    	  );
    	  // Underflow Exceptions
	  // Declare global message for types of exceptions
    	  numOfUnderflow ++;
    	  std::stringstream underFlowStream;
    	  underFlowStream << "Potential Underflow " << numOfUnderflow << "d \n";
    	  Constant *underFlowMsg = ConstantArray::get(
    			  I->getContext(), underFlowStream.str().c_str(), true
    	  );

    	  GlobalVariable *underFlowString = new GlobalVariable(
    			  *I->getParent()->getParent()->getParent(),
    			  underFlowMsg->getType(),
    			  true,
    			  GlobalValue::InternalLinkage,
    			  underFlowMsg,
    			  "underflow"
    	  );
	  if (!isConcrete) {
    		  Constant *location = ConstantInt::get(
    				  Type::getInt32Ty(I->getContext()), numOfUnderflow
    		  );
    		  setLocationCall(location, underFlowBB);
    		  raiseExceptionCall(FE_UNDERFLOW, underFlowBB);
    	  }
    	  printErrMsg(underFlowString,underFlowBB);
	  if (!isConcrete) {
    		  captureExprCall (param, postBlock);
    		  LoadInst *klee_symbolic = klee_call(
    			  __exp_tmp, postBlock, I->getType()
    		  );

    		  I->replaceAllUsesWith(klee_symbolic->getUnderlyingObject());
    		  I->eraseFromParent();
    	  } else {
    		  I->removeFromParent();
    		  postBlock->getInstList().push_back(I);
    	  }
      }

      /* Rewrite log into new semantics
       * Note: log (base e) can overflow & underflow, can have invalid, but
       * no division by zero.
       * log(x) =>
       *   1. x <= 0 => invalid
       *   2. 0 < x < exp(-max) => overflow
       *   3. 1 < x < exp(min) => underflow
       *   4. generare fresh symbolic variable to denote log(x)
       *     a. x = 1 && y = 0
       *     b. x > 1 && y > 0
       *     c. 0 < x < 1 && y < 0
       */
      void handleLog (
    		  Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock
      ) {

    	  Value* zeroFP = getZeroFP(I->getType(), I->getContext());
    	  Value *param = I->getOperand(1);

    	  CmpInst *paramGtZeroOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_UGT,
    			  param, zeroFP,"", preBlock
    	  );

    	  BasicBlock *invalidBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  invalidBB->moveAfter(preBlock);


    	  BranchInst::Create(
    			  postBlock, invalidBB,
    			  paramGtZeroOp->getUnderlyingObject(), preBlock
    	  );
    	  // Invalid Exception
    	  //Declare global message for types of exceptions
    	  numOfInvalid ++;
    	  std::stringstream invalidStream;
    	  invalidStream << "Potential Invalid " << numOfInvalid << "d \n";
    	  Constant *invalidMsg = ConstantArray::get(
    			  I->getContext(), invalidStream.str().c_str(), true
    	  );

    	  GlobalVariable *invalidString = new GlobalVariable(
    			  *I->getParent()->getParent()->getParent(),
    			  invalidMsg->getType(),
    			  true,
    			  GlobalValue::InternalLinkage,
    			  invalidMsg,
    			  "invalid"
    	  );
    	  if(!isConcrete) {
    		  Constant *location = ConstantInt::get(
    				  Type::getInt32Ty(I->getContext()), numOfInvalid
    		  );
    		  setLocationCall(location, invalidBB);
    		  raiseExceptionCall(FE_INVALID, invalidBB);
    	  }
    	  printErrMsg(invalidString,invalidBB);
    	  if (!isConcrete) {
    		  captureExprCall (param, postBlock);
    		  LoadInst *klee_symbolic = klee_call(
    			  __log_tmp, postBlock, I->getType()
    		  );

    		  I->replaceAllUsesWith(klee_symbolic->getUnderlyingObject());
    		  I->eraseFromParent();
    	  } else {
    		  I->removeFromParent();
    		  postBlock->getInstList().push_back(I);
    	  }

      }

      /* Rewrite sin into new semantics
       * Note: There are no exception in this case
       * sin(x) =>
       *   1. generare fresh symbolic variable to denote sin(x)
       *     -1 <= sin(x) <= 1
       */
      void handleSin (
    		  Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock
      ) {
    	  Value* oneFP = getOneFP(I->getType(), I->getContext());
    	  Value* moneFP = getmOneFP(I->getType(), I->getContext());
    	  Value* param = I->getOperand(1);
    	  LoadInst *load_symbolic;
    	  if (!isConcrete) {
    		  captureExprCall (param,preBlock);
    		  load_symbolic = klee_call(
    			  __sin_tmp, preBlock,I->getType()
    		  );
    	  } else {
    		  I->removeFromParent();
    		  preBlock->getInstList().push_back(I);
    	  }
    	  CmpInst *resLtmOneOp;
    	  if (!isConcrete)
    		  resLtmOneOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_ULT,
    			  load_symbolic->getUnderlyingObject(), moneFP, "", preBlock
    		  );
    	  else
    		  resLtmOneOp = CmpInst::Create(
    			 Instruction::FCmp, FCmpInst::FCMP_ULT,
    			 I->getUnderlyingObject(), moneFP, "", preBlock
    		   );

    	  BasicBlock *resGtOneBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  resGtOneBB->moveAfter(preBlock);
    	  BasicBlock *unreachableBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  unreachableBB->moveAfter(resGtOneBB);
    	  postBlock->moveAfter(unreachableBB);

    	  BranchInst::Create(
    			  unreachableBB, resGtOneBB,
    			  resLtmOneOp->getUnderlyingObject(), preBlock
    	  );

    	  CmpInst *resGtOneOp;
	  if(!isConcrete)
    		  resGtOneOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_UGT,
    			  load_symbolic->getUnderlyingObject(), oneFP, "", resGtOneBB
    		  );
    	  else
    		  resGtOneOp = CmpInst::Create(
    				  Instruction::FCmp, FCmpInst::FCMP_UGT,
    				  I->getUnderlyingObject(), oneFP, "", resGtOneBB
    		  );

    	  BranchInst::Create(
    			  unreachableBB, postBlock,
    			  resGtOneOp->getUnderlyingObject(), resGtOneBB
    	  );

    	  new UnreachableInst(
    			  preBlock->getContext(),unreachableBB
    	  );
	  if (!isConcrete) {
    		  I->replaceAllUsesWith(load_symbolic->getUnderlyingObject());
			  I->eraseFromParent();
    	  }
      }

      /* Rewrite cos into new semantics
       * Note: There are no exception in this case
       * cos(x) =>
       *   1. generare fresh symbolic variable to denote sin(x)
       *     -1 <= cos(x) <= 1
       */
      void handleCos (
    		  Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock
      ) {
    	  Value* oneFP = getOneFP(I->getType(), I->getContext());
    	  Value* moneFP = getmOneFP(I->getType(), I->getContext());
    	  Value* param = I->getOperand(1);
    	  LoadInst *load_symbolic;
	  if (!isConcrete) {
    		  captureExprCall (param, preBlock);
    		  load_symbolic = klee_call(
    			  __cos_tmp,preBlock, I->getType()
    		  );
    	  } else {
    		  I->removeFromParent();
    		  preBlock->getInstList().push_back(I);
    	  }
    	  CmpInst *resLtmOneOp;
    	  if (!isConcrete)
    		  resLtmOneOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_ULT,
    			  load_symbolic->getUnderlyingObject(), moneFP, "", preBlock
    		  );
    	  else
    		  resLtmOneOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_ULT,
    		      I->getUnderlyingObject(), moneFP, "", preBlock
    		  );

    	  BasicBlock *resGtOneBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  resGtOneBB->moveAfter(preBlock);
    	  BasicBlock *unreachableBB = BasicBlock::Create(
    			  preBlock->getContext(),"",preBlock->getParent()
    	  );
    	  unreachableBB->moveAfter(resGtOneBB);
    	  postBlock->moveAfter(unreachableBB);

    	  BranchInst::Create(
    			  unreachableBB,resGtOneBB,
    			  resLtmOneOp->getUnderlyingObject(), preBlock
    	  );

    	  CmpInst *resGtOneOp;
	  if (!isConcrete)
    		  resGtOneOp= CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_UGT,
    			  load_symbolic->getUnderlyingObject(), oneFP, "", resGtOneBB
    		  );
    	  else
    		  resGtOneOp= CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_UGT,
    			  I->getUnderlyingObject(), oneFP, "", resGtOneBB
    		  );

    	  BranchInst::Create(
    			  unreachableBB, postBlock,
    			  resGtOneOp->getUnderlyingObject(), resGtOneBB
    	  );

    	  new UnreachableInst(
    			  preBlock->getContext(),unreachableBB
    	  );
	  if (!isConcrete) {
    		  I->replaceAllUsesWith(load_symbolic->getUnderlyingObject());
    		  I->eraseFromParent();
    	  }
      }

      void processCall(
    		  Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock
      ) {
    	  StringRef functionName = I->getOperand(0)->getName();
    	  if(functionName.compare("sqrt") == 0)
			  handleSqrt(I,preBlock,postBlock);
    	  else if (functionName.compare("pow") == 0
    			  || functionName.compare("llvm.pow.f64") == 0)
			  handlePow(I, preBlock, postBlock);
    	  else if (functionName.compare("exp") == 0
    			  || functionName.compare("llvm.exp.f64") == 0){
    		  handleExp(I, preBlock, postBlock);
    	  }
    	  else if (functionName.compare("log") == 0
    			  || functionName.compare("llvm.log.f64") == 0){
    		  handleLog(I, preBlock, postBlock);
    	  }else if (functionName.compare("sin") == 0
    			  || functionName.compare("llvm.sin.f64") == 0){
    		  handleSin(I, preBlock, postBlock);
    	  }else if (functionName.compare("cos") == 0
    			  || functionName.compare("llvm.cos.f64") == 0){
    		  handleCos(I, preBlock, postBlock);
    	  }
      }

      void processConvert(
    		  Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock
      ) {
    	  double iMax = INT_MAX;
    	  if (I->getType() == Type::getInt8Ty(I->getContext()))
    		  iMax = CHAR_MAX;
    	  else if (I->getType() == Type::getInt16Ty(I->getContext()))
    		  iMax = SHRT_MAX;
	  
    	  Value *operand = I->getOperand(0);
    	  Value *max = ConstantFP::get(
    			  operand->getType(), iMax
    	  );
    	  Value *nmax = ConstantFP::get(
    			  operand->getType(), -iMax
    	  );

    	  

    	  // Move the instruction to previous basic block
    	  I->removeFromParent();
    	  postBlock->getInstList().push_back(I);

    	  // Compare with max
    	  CmpInst *sgtMaxOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_OGT,
    			  operand, max, "", preBlock
    	  );

    	  // Compare with -max
    	  CmpInst *sltnMaxOp = CmpInst::Create(
    			  Instruction::FCmp, FCmpInst::FCMP_OLT,
    			  operand, nmax, "", preBlock
    	  );

    	  // Or operation
    	  BinaryOperator *orCond = BinaryOperator::Create(
    			  Instruction::Or, sgtMaxOp, sltnMaxOp, "", preBlock
    	  );

    	  BasicBlock *overFlowBB = BasicBlock::Create(
    			  preBlock->getContext(), "", preBlock->getParent()
    	  );
    	  overFlowBB->moveAfter(preBlock);
    	  BranchInst::Create(
    	  		  overFlowBB, postBlock,
    	  		  orCond->getUnderlyingObject(), preBlock
    	  );

    	  //Overflow Exception Case
    	  //Declare global message for types of exceptions
    	  numOfOverflow ++;
    	  std::stringstream overFlowStream;
    	  overFlowStream << "Potential Overflow " << numOfOverflow << " \n";
    	  Constant *overFlowMsg = ConstantArray::get(
    			  I->getContext(), overFlowStream.str().c_str(), true
    	  );

    	  GlobalVariable *overFlowString = new GlobalVariable(
    			  *I->getParent()->getParent()->getParent(),
    			  overFlowMsg->getType(),
    			  true,
    			  GlobalValue::InternalLinkage,
    			  overFlowMsg,
    			  "overflow"
		 );
	 if (!isConcrete) {
    		 Constant *location = ConstantInt::get(
    				 Type::getInt32Ty(I->getContext()), numOfOverflow
    		 );
    		 setLocationCall(location, overFlowBB);
    		 raiseExceptionCall(FE_OVERFLOW, overFlowBB);
    	 }
    	 printErrMsg(overFlowString,overFlowBB);
      }

	  /*
	   * Process an instruction with preBlock is the current block that
	   * the instruction is its last instruction
	   */
      void processInstruction(
    		  Instruction *I, BasicBlock *preBlock, BasicBlock *postBlock
      ) {

	      	switch (I->getOpcode()){
	  			case Instruction::Call:
	  				processCall(I,preBlock,postBlock);
	  				break;

	  			//Floating point arithmetic operations
	  			case Instruction::FAdd: {
	  				processOp(I,preBlock,postBlock);
	  				break;
	  			}

	  			case Instruction::FSub: {
	  				processOp(I,preBlock,postBlock);
	  				break;
	  			}

	  			case Instruction::FMul: {
					if (isConcrete)
						processMultOp(I, preBlock, postBlock);
					else
						processOp(I,preBlock,postBlock);
	  				break;
	  			}

	  			case Instruction::FDiv: {
	  				processDivOp(I, preBlock, postBlock);
	  				break;
	  			}
	  			case Instruction::FPToSI:
	  				processConvert(I, preBlock, postBlock);
	  				break;
	  			case Instruction::FPToUI:
	  				processConvert(I, preBlock, postBlock);
	  				break;
	  			default:
	  				//B->getInstList().push_back(&I);
	  				break;
	      	}
	  }

	  /* Determine if an instruction need to be changed or not
	   * FIXME: this time we only run with double numbers
	   * Need to add handling for all floating point types
	   */
      bool needToTransform(Instruction *I){
		  if (I->getNumOperands() < 1)
			  return false;
		  StringRef name = I->getOperand(0)->getName();
		  switch ( I->getOpcode()){
			  /* Floating point arithmetic operations */
			  case Instruction::FAdd:
			  case Instruction::FSub:
			  case Instruction::FMul:
			  case Instruction::FDiv:
				  return true;
			  case Instruction::FPToSI:
			  case Instruction::FPToUI:
				  return true;

			 /* List of function need to be processed: sqrt, pow, exp, log, sin,
			  * cos and other trignometric functions
			  */
			  case Instruction::Call:

				  if(name.compare("sqrt") == 0
					|| name.compare("pow") == 0
					|| name.compare("llvm.pow.f64") == 0
					|| name.compare("exp") == 0
					|| name.compare("llvm.exp.f64") == 0
					|| name.compare("log") == 0
					|| name.compare("llvm.log.f64") == 0
					|| name.compare("sin") == 0
					|| name.compare("llvm.sin.f64") == 0
					|| name.compare("cos") == 0
					|| name.compare("llvm.cos.f64") == 0)
					  return true;
				  else
					return false;

			  default:
				  return false;
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

	  /*Traverse a basic block, for each instruction transform into a list of
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
			  if (needToTransform(I)){
				  BasicBlock *postBlock = BasicBlock::Create(
						  preBlock->getContext(), "",
						  preBlock->getParent(), nextBB
				  );
				  postBlock->moveBefore(nextBB);
				  processInstruction(I, preBlock, postBlock);
				  preBlock = postBlock;
			  }
			  else {
				  I->removeFromParent();
				  preBlock->getInstList().push_back(I);
			  }
			  I = next;
		  }
		  // Set the references in PHI nodes for B by preBlock
		  replacePHIReferences(firstBB,preBlock);
		  B->removeFromParent();
	  }

	  virtual bool runOnFunction(Function &F){
		  StringRef name = F.getName();
		  // Ignore some functions
		  if (name.compare("fabs") == 0 || name.compare("fabsf") == 0)
			  return false;
		  Function::iterator BB = F.begin();
		  BasicBlock *B = BB;
		  BasicBlock *endBB = B;

		  while (BB != F.end()) {
			  endBB = BB;
			  BB++;
		  }
		  bool terminate = false;
		  BB = F.begin();
		  B = BB;

		  while ( BB != F.end() && !terminate){
			  /*
			   * Get the next basic block to process in the list before changing
			   * the content of basic block
			   */
			  if (B == endBB)
				  terminate = true;
			  BasicBlock *next = ++BB;
			  processBlock(B,next);
			  B = next;
		  }
		  //F.print(errs(),&w); // debugging
		  return true;
	  }

	  /*
	   * Get constants functions based on the type of instructions	   *
	   */

	  //FIXME: should return the correct max, min values of corresponding types
	  Constant* getMax(const Type *type,  LLVMContext &C){
		  if (type == Type::getFloatTy(C)){
			  return ConstantFP::get(Type::getFloatTy(C),FLT_MAX);
		  } else
			  return ConstantFP::get(type,DBL_MAX);
	  }

	  Constant* getMin(const Type *type,  LLVMContext &C){
		  if (type == Type::getFloatTy(C)){
			  return ConstantFP::get(Type::getFloatTy(C),FLT_MIN);
		  } else
			  return ConstantFP::get(type,DBL_MIN);
	  }

	  Constant *getZeroFP(const Type *type, LLVMContext &C) {
		  return ConstantFP::get(type, 0);
	  }

	  Constant *getOneFP(const Type *type, LLVMContext &C) {
	  	  return ConstantFP::get(type, 1);
	  }

	  Constant *getmOneFP(const Type *type, LLVMContext &C) {
	  	  return ConstantFP::get(type, -1);
	  }

	  Constant* getLogMax(const Type *type, LLVMContext &C){
		  if (type == Type::getFloatTy(C)){
			  return ConstantFP::get(Type::getFloatTy(C),log(FLT_MAX));
		  } else
			  return ConstantFP::get(type,log(DBL_MAX));
	  }

	  Constant* getLogMin(const Type *type, LLVMContext &C){
		  if (type == Type::getFloatTy(C)){
			  return ConstantFP::get(Type::getFloatTy(C),log(FLT_MIN));
		  } else
			  return ConstantFP::get(type,log(DBL_MIN));
	  }

	  Constant* getExpnMax(const Type *type, LLVMContext &C){
	  	  if (type == Type::getFloatTy(C)){
	  		  return ConstantFP::get(Type::getFloatTy(C),exp(-FLT_MAX));
	  	  } else
	  		  return ConstantFP::get(type,exp(-DBL_MAX));
	  }

  	  Constant* getExpMin(const Type *type, LLVMContext &C){
  		  if (type == Type::getFloatTy(C)){
  			  return ConstantFP::get(Type::getFloatTy(C), exp(FLT_MIN));
  		  } else
  			  return ConstantFP::get(type, exp(DBL_MIN));
  	  }

	  // Initialize constants and function declarations for modules
	  virtual bool doInitialization(Module &M){

		  // Set the normal mode by getting the value of environment variable __ARIADNE_NORMAL
		  if (getenv("__FORTRAN") == NULL)
			  isConcrete = false;
		  else {
			  int normal = atoi(getenv("__ARIADNE_NORMAL"));
			  if (normal == 1)
				  isConcrete = true;
			  else
				  isConcrete = false;
		  }

		  numOfOverflow = 0;
		  numOfUnderflow = 0;
		  numOfDivisionByZero = 0;
		  numOfInvalid = 0;

		  Constant *ariadne_tmp_msg = ConstantArray::get(
				  M.getContext(), "__ariadne_tmp", true
		  );

		  __ariadne_tmp = new GlobalVariable(
				  M,
				  ariadne_tmp_msg->getType(),
				  true,
				  GlobalValue::InternalLinkage,
				  ariadne_tmp_msg,
				  "__ariadne_tmp"
		  );

		  Constant *sqrt_tmp_msg = ConstantArray::get(
				  M.getContext(), "__sqrt_tmp", true
		  );

		  __sqrt_tmp = new GlobalVariable(
				  M,
				  sqrt_tmp_msg->getType(),
				  true,
				  GlobalValue::InternalLinkage,
				  sqrt_tmp_msg,
				  "__sqrt_tmp"
		  );

		  Constant *exp_tmp_msg = ConstantArray::get(
				  M.getContext(), "__exp_tmp", true
		  );

		  __exp_tmp = new GlobalVariable(
				  M,
				  exp_tmp_msg->getType(),
				  true,
				  GlobalValue::InternalLinkage,
				  exp_tmp_msg,
				  "__exp_tmp"
		  );

		  Constant *log_tmp_msg = ConstantArray::get(
				  M.getContext(), "__log_tmp", true
		  );

		  __log_tmp = new GlobalVariable(
				  M,
				  log_tmp_msg->getType(),
				  true,
				  GlobalValue::InternalLinkage,
				  log_tmp_msg,
				  "__log_tmp"
		  );

		  Constant *sin_tmp_msg = ConstantArray::get(
				  M.getContext(), "__sin_tmp", true
		  );

		  __sin_tmp = new GlobalVariable(
				  M,
				  sin_tmp_msg->getType(),
				  true,
				  GlobalValue::InternalLinkage,
				  sin_tmp_msg,
				  "__sin_tmp"
		  );

		  Constant *cos_tmp_msg = ConstantArray::get(
				  M.getContext(), "__cos_tmp", true
		  );

		  __cos_tmp = new GlobalVariable(
				  M,
				  cos_tmp_msg->getType(),
				  true,
				  GlobalValue::InternalLinkage,
				  cos_tmp_msg,
				  "__cos_tmp"
		  );

		  Constant *pow_tmp_msg = ConstantArray::get(
				  M.getContext(), "__pow_tmp", true
		  );

		  __pow_tmp = new GlobalVariable(
				  M,
				  pow_tmp_msg->getType(),
				  true,
				  GlobalValue::InternalLinkage,
				  pow_tmp_msg,
				  "__pow_tmp"
		  );

		  Constant *infeasible_msg = ConstantArray::get(
				  M.getContext(), "Infeasible Path\n", true
		  );

		  infeasible_Path = new GlobalVariable(
				  M,
				  infeasible_msg->getType(),
				  true,
				  GlobalValue::InternalLinkage,
				  infeasible_msg,
				  "infeasible_Path"
		  );

		  //Set DBL_MAX, DBL_MIN, zero constants
		  dbl_max = ConstantFP::get(Type::getDoubleTy(M.getContext()),DBL_MAX);
		  dbl_min = ConstantFP::get(Type::getDoubleTy(M.getContext()),DBL_MIN);
		  zero_dbl = ConstantFP::get(Type::getDoubleTy(M.getContext()),0);
		  one_dbl = ConstantFP::get(Type::getDoubleTy(M.getContext()),1);
		  log_DBLMAX = ConstantFP::get(
				  Type::getDoubleTy(M.getContext()),log(DBL_MAX)
		  );
		  log_DBLMIN = ConstantFP::get(
				  Type::getDoubleTy(M.getContext()),log(DBL_MIN)
		  );
		  exp_nDBLMAX = ConstantFP::get(
				  Type::getDoubleTy(M.getContext()),exp(-DBL_MAX)
		  );
		  exp_DBLMIN = ConstantFP::get(
				  Type::getDoubleTy(M.getContext()),exp(DBL_MIN)
		  );

		  flt_max = ConstantFP::get(Type::getFloatTy(M.getContext()),FLT_MAX);
		  flt_min = ConstantFP::get(Type::getFloatTy(M.getContext()),FLT_MIN);
		  zero_flt = ConstantFP::get(Type::getFloatTy(M.getContext()),0);
		  one_flt = ConstantFP::get(Type::getFloatTy(M.getContext()),1);
		  log_FLTMAX = ConstantFP::get(
		  		  Type::getFloatTy(M.getContext()),log(FLT_MAX)
		  );

		  log_FLTMIN = ConstantFP::get(
				  Type::getFloatTy(M.getContext()),log(FLT_MIN)
		  );
		  exp_nFLTMAX = ConstantFP::get(
				  Type::getFloatTy(M.getContext()),exp(-FLT_MAX)
		  );
		  exp_FLTMIN = ConstantFP::get(
	  			  Type::getFloatTy(M.getContext()),exp(FLT_MIN)
		  );

		  FE_OVERFLOW = ConstantInt::get(
				  Type::getInt32Ty(M.getContext()), 0
		  );

		  FE_UNDERFLOW = ConstantInt::get(
				  Type::getInt32Ty(M.getContext()), 1
		  );

		  FE_INVALID = ConstantInt::get(
				  Type::getInt32Ty(M.getContext()), 2
		  );

		  FE_DIVBYZERO = ConstantInt::get (
				  Type::getInt32Ty(M.getContext()), 3
		  );

		  int_max = ConstantFP::get(
	  			  Type::getFloatTy(M.getContext()),INT_MAX
		  );

		  //Declare fabs function
		  dbl_fabs = cast<Function>(M.getOrInsertFunction(
				  "fabs", Type::getDoubleTy(M.getContext()),
				  Type::getDoubleTy(M.getContext()), NULL)
		  );

		  flt_fabs = cast<Function>(M.getOrInsertFunction(
				  "fabsf", Type::getFloatTy(M.getContext()),
				  Type::getFloatTy(M.getContext()), NULL)
		  );

		  //Declare exit function
		  exit = cast<Function>(M.getOrInsertFunction(
				  "exit", Type::getVoidTy(M.getContext()),
				  Type::getInt32Ty(M.getContext()), NULL)
		  );

		  /// Set linkage for injected and defined functions
		  dbl_fabs->setLinkage(llvm::Function::LinkOnceODRLinkage);
		  flt_fabs->setLinkage(llvm::Function::LinkOnceODRLinkage);

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

		  if (!isConcrete) {
			  klee_make_symbolic = cast<Function>(M.getOrInsertFunction (
				  "klee_make_symbolic",
				  Type::getVoidTy(M.getContext()),
				  PointerType::getUnqual(IntegerType::getInt8Ty(M.getContext())),
				  Type::getInt32Ty(M.getContext()),
				  PointerType::getUnqual(IntegerType::getInt8Ty(M.getContext())),
				  NULL)
			  );

			  captureExpr = cast<Function> (M.getOrInsertFunction(
				  "captureExpr",
				  Type::getVoidTy(M.getContext()),
				  Type::getDoubleTy(M.getContext()),
				  NULL)
			  );

			  raiseException = cast <Function> (M.getOrInsertFunction(
				  "raiseException",
				  Type::getVoidTy(M.getContext()),
				  Type::getInt32Ty(M.getContext()), NULL)
			  );

			  setExceptionLocation = cast <Function> (M.getOrInsertFunction(
					  "setExceptionLocation",
					  Type::getVoidTy(M.getContext()),
					  Type::getInt32Ty(M.getContext()), NULL)
			  );

		  }

		  //Adding definition for fabs
		  if (dbl_fabs->isDeclaration()) {
			  BasicBlock *ogeZeroBB = BasicBlock::Create(
					  M.getContext(), "", dbl_fabs
			  );
			  Value *param = dbl_fabs->getArgumentList().begin();
			  //Set name for parameter
			  param->setName("x");

			  // Compare with Zero
			  CmpInst *ogeZeroOp = CmpInst::Create(
					  Instruction::FCmp, FCmpInst::FCMP_OGT,
					  param, zero_dbl, "", ogeZeroBB
			  );

			  BasicBlock *posReturnBB = BasicBlock::Create(
					  M.getContext(), "", dbl_fabs
			  );

			  ReturnInst::Create(
					  M.getContext(), param, posReturnBB
			  );

			  BasicBlock *negReturnBB = BasicBlock::Create(
					  M.getContext(), "", dbl_fabs
			  );

			  BranchInst::Create(
					  posReturnBB, negReturnBB,
					  ogeZeroOp->getUnderlyingObject(), ogeZeroBB
			  );

			  BinaryOperator *subOp = BinaryOperator::Create(
					  Instruction::Sub,
					  zero_dbl, param, "", negReturnBB
			  );

			  ReturnInst::Create(
					  M.getContext(), subOp->getUnderlyingObject(), negReturnBB
			  );
		  }

		  //Adding definition for fabs
		  if (flt_fabs->isDeclaration()) {
			  BasicBlock *ogeZeroBB = BasicBlock::Create(
					  M.getContext(), "", flt_fabs
			  );
			  Value *param = flt_fabs->getArgumentList().begin();
			  //Set name for parameter
			  param->setName("x");

			  // Compare with Zero
			  CmpInst *ogeZeroOp = CmpInst::Create(
					  Instruction::FCmp, FCmpInst::FCMP_OGT,
					  param, zero_flt, "", ogeZeroBB
			  );

			  BasicBlock *posReturnBB = BasicBlock::Create(
					  M.getContext(), "", flt_fabs
			  );

			  ReturnInst::Create(
					  M.getContext(), param, posReturnBB
			  );

			  BasicBlock *negReturnBB = BasicBlock::Create(
					  M.getContext(), "", flt_fabs
			  );

			  BranchInst::Create(
					  posReturnBB, negReturnBB,
					  ogeZeroOp->getUnderlyingObject(), ogeZeroBB
			  );

			  BinaryOperator *subOp = BinaryOperator::Create(
					  Instruction::Sub,
					  zero_flt, param, "", negReturnBB
			  );

			  ReturnInst::Create(
					  M.getContext(), subOp->getUnderlyingObject(), negReturnBB
			  );
		  }
		  return true;
	  }

	  // Print out the module for debugging
	  virtual bool doFinalization(Module &M) {
		  M.print(errs(),&w);
		  return true;
	  }

  };
}

char Ariadne::ID = 0;
static RegisterPass<Ariadne> X("Ariadne", "Floating point exceptions Pass");

// Publically exposed interface to pass...
const PassInfo *const llvm::AriadneID = &X;

