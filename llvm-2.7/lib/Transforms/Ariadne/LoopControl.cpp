/*  Conceptually, this transformation rewrites all loops into for loops whose
 *  upper bound is "bound_max".  Other than the additional conjunct that tests
 *  the number of iterations against the bound, the original loop predicate is
 *  unchanged.  The point is to bound the execution of loops on which symbolic
 *  execution might otherwise diverge.
 *
 *  This transformation is not semantics-preserving over concrete execution.
 *  Its purpose is to facilitate symbolic execution over a bounded computation
 *  tree.  It generalizes ESC Java practice of unrolling loops 1.5 times.
 */
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/STLExtras.h"

#include "Ariadne.h"

using namespace llvm;

namespace {
  struct LoopControl : public LoopPass {
    static char ID; // Pass identification, replacement for typeid
    LoopControl() : LoopPass(&ID) {}

    Function *mainFn; //Main function for setting MAX_BOUND
    Function *setBound;
    Function *__getenv;
    GlobalVariable *max_bound; //Upper bound injected into loops
    GlobalVariable *max_boundString;
    //FIXME: need to replace bound_max by max_bound
    //Constant *bound_max;  //Upper bound injected into loops.

    // FIXME: fix the transformations for setMaxBound internally
    virtual bool doInitialization(Loop *loop, LPPassManager &LPM){
		// Unfortunately, this environment variable is a magic constant used
		// to allow a user to pass in a maximum bound.  Ideally, this value
		// would be passed on the command line, but llvm offers no such
		// facility for passes.
		// To do this transformation, in the input code we should include
    	// Ariadne.h

    	Module *module = loop->getHeader()->getParent()->getParent();

    	Constant *zero_32 = Constant::getNullValue(
    			Type::getInt32Ty(module->getContext())
    	);

    	//Declare getenv function
    	FunctionType* ft_getenv = FunctionType::get(
    			IntegerType::getInt32Ty(module->getContext()), true
    	);

    	__getenv = cast<Function>(
    			module->getOrInsertFunction("getenv", ft_getenv)
    	);

    	//To perform LoopControl transformation, the module already had a main


    	//mainFn = cast<Function> (module->getFunction("main"));

    	mainFn = cast<Function> (module->getOrInsertFunction(
				"main",IntegerType::getInt32Ty(module->getContext()), NULL)
		);

    	if ( mainFn->isDeclaration()) {

    		max_bound = new GlobalVariable(
    				*module,
    				zero_32->getType(),
    				true,
    				GlobalValue::InternalLinkage,
    				zero_32,
    				"max_bound"
    		);

    		Constant *max_bound_msg = ConstantArray::get(
    				module->getContext(), "__ARIADNE_BOUNDMAX", true
    		);

    		max_boundString = new GlobalVariable(
    				*module,
    				max_bound_msg->getType(),
    				true,
    				GlobalValue::InternalLinkage,
    				max_bound_msg,
    				"max_boundString"
    		);

    		BasicBlock *mainEntry = BasicBlock::Create(
    			module->getContext(), "", mainFn, mainFn->begin()
    		);

    		//Instruction *firstInst = mainFn->getEntryBlock().getFirstNonPHI();

    		Constant *gep_params[] = {
    			zero_32,
    			zero_32
    		};

    		Constant *param = ConstantExpr::getGetElementPtr(
    			max_boundString, gep_params, array_lengthof(gep_params)
    		);

    		Value *params[] = {
    	    	param
    		};

    		CallInst *get_env = CallInst::Create(
    			__getenv,	params, array_endof(params), "", mainEntry
    		);

    		new StoreInst(get_env, max_bound, mainEntry);
    		ReturnInst::Create(module->getContext(), zero_32, mainEntry);
    	}

    	/*mainFn = cast<Function> (module->getOrInsertFunction(
				"main",IntegerType::getInt32Ty(module->getContext()), NULL)
		);

		BasicBlock *mainEntry = BasicBlock::Create(
				mainFn->getContext(), "",mainFn
		);

		setBound = cast<Function>(module->getOrInsertFunction(
				"setBound", Type::getVoidTy(module->getContext()), NULL)
		);

		CallInst::Create(setBound,
				"", mainEntry
		);

		new UnreachableInst(module->getContext(), mainEntry);*/

		/*char* bound_env = getenv("__ARIADNE_BOUNDMAX");
		int value = 8;
		if (bound_env != NULL)
			value = atoi(bound_env);
		bound_max = ConstantInt::get(
				Type::getInt32Ty(loop->getHeader()->getContext()),
				value
		);*/
		return false;
    }

	/* Initialize lcv to zero in a basic block predecessor of the loop control
	 * block, for all predecessors outside the loop.  This function assumes
	 * that the loop body jumps unconditionally back to the loop control.
	 */
	void zeroLCV(
			BasicBlock *loopheader, BasicBlock *jumpBody, AllocaInst *lcv,
			Constant *zero_32
	) {
		for (pred_iterator PI = pred_begin(loopheader),
			 E = pred_end(loopheader);
			 PI != E; ++PI)
		{
			BasicBlock *Pred = *PI;
			if (jumpBody == Pred) continue;
			BranchInst *preBI = dyn_cast<BranchInst>(Pred->getTerminator());
			StoreInst *lcv_store = new StoreInst(
				zero_32, lcv->getUnderlyingObject(), false, 4, Pred
			);
			lcv_store->moveBefore(preBI);
		}
	}

	/* Add instructions to the loop header to load and compare our loop control
	 * variable against "bound_max".  It then increments and stores the loop
	 * control variable for the next iteration.  We inject this load and store
	 * into the loop header, not the loop body, which appears to be the llvm
	 * convention.
	 */
	CmpInst * updateLCV(
			AllocaInst *lcv, BranchInst *lhTerminator, BasicBlock *loopheader
	) {
		LoadInst *lcv_load = new LoadInst(
			lcv->getUnderlyingObject(), "", false, 4,
			loopheader
		);
		lcv_load->moveBefore(lhTerminator);

		LoadInst *loadMax = new LoadInst(
			max_bound->getUnderlyingObject(),"",lhTerminator
		);
		CmpInst *ultCmp = CmpInst::Create(
			Instruction::ICmp, FCmpInst::ICMP_ULT,
			lcv_load->getUnderlyingObject(),
			loadMax->getUnderlyingObject(), "", loopheader
		);
		ultCmp->moveBefore(lhTerminator);
		BinaryOperator *incr = BinaryOperator::CreateNUWAdd(
			lcv_load->getUnderlyingObject(),
			ConstantInt::get(
				IntegerType::getInt32Ty(loopheader->getContext()), 1
			),
			"", loopheader
		);
		incr->moveBefore(lhTerminator);
		StoreInst *lcv_store = new StoreInst(
			incr->getUnderlyingObject(),
			lcv->getUnderlyingObject(),
			false, 4, loopheader
		);
		lcv_store->moveBefore(lhTerminator);
		return ultCmp;
	}

	bool transformUnconditionalLoop(
			Loop *loop, BasicBlock *loopheader, BranchInst *lhTerminator,
			AllocaInst *loop_control_variable, Constant *zero_32
	) {

		BasicBlock *jumpBody = lhTerminator->getSuccessor(0);
		zeroLCV(loopheader, jumpBody, loop_control_variable, zero_32);
		SmallVector<BasicBlock*, 4> exitBlocks;
		loop->getExitBlocks(exitBlocks);
		assert(exitBlocks.size() != 0);
		CmpInst *ultCmp =
			updateLCV(loop_control_variable, lhTerminator, loopheader
		);
		BranchInst::Create(
			lhTerminator->getSuccessor(0),
			exitBlocks[0], // First loop exit
			ultCmp->getUnderlyingObject(), loopheader
		);
		lhTerminator->eraseFromParent();

		return true;
	}

	bool transformConditionalLoop(
			Loop *loop, BasicBlock *loopheader, BranchInst *lhTerminator,
			AllocaInst *loop_control_variable, Constant *zero_32
	) {
		// Find which label of loopheader's terminator is the loop body.
		BasicBlock *thenBody = lhTerminator->getSuccessor(0);
		int isThenLoopBody = 1;
		SmallVector<BasicBlock*, 4> exitBlocks;
		loop->getExitBlocks(exitBlocks);
		for (unsigned i = 0, e = exitBlocks.size(); i != e; ++i) {
			const BasicBlock *exitBlock = exitBlocks[i];
			if (exitBlock == thenBody) {
				isThenLoopBody = 0;
				break;
			}
		}
		BasicBlock *jumpBody;
		if (isThenLoopBody) {
			jumpBody = thenBody;
		}
		else {
			jumpBody = lhTerminator->getSuccessor(1);
		}

		zeroLCV(loopheader, jumpBody, loop_control_variable, zero_32);

		CmpInst *ultCmp =
			updateLCV(loop_control_variable, lhTerminator, loopheader
		);

		// Rewrite the existing loop control's branch to include our check as a
		// conjunct.
		Value *lcCond = lhTerminator->getCondition();
		BinaryOperator *lcand;
		if (isThenLoopBody) {
			lcand = BinaryOperator::Create(
				Instruction::And,
				lcCond->getUnderlyingObject(),
				ultCmp->getUnderlyingObject(),
				"", loopheader
			);
		}
		else { //else basic block is the loop body
			BinaryOperator *logicalNot = BinaryOperator::CreateNot(
				ultCmp->getUnderlyingObject(), "", loopheader
			);
			logicalNot->moveBefore(lhTerminator);
			lcand = BinaryOperator::Create(
				Instruction::And,
				lcCond->getUnderlyingObject(),
				logicalNot->getUnderlyingObject(),
				"", loopheader
			);
		}
		lcand->moveBefore(lhTerminator);
		lhTerminator->setCondition(lcand->getUnderlyingObject());

		return true;
	}

	virtual bool runOnLoop(Loop *loop, LPPassManager &LPM) {
		bool ret = false;
		BasicBlock *loopheader = loop->getHeader();
		BranchInst *lhTerminator =
			dyn_cast<BranchInst>(loopheader->getTerminator());

		// Allocate space for a loop control variable.  This allocation is
		// inefficient in the sense that each loop has its own loop control
		// variable. Sibling loops, which could share a loop control variable,
		// do not.
		BasicBlock *entry = &loopheader->getParent()->getEntryBlock();
		AllocaInst *loop_control_variable = new AllocaInst(
			Type::getInt32Ty(loopheader->getContext()), "", entry
		);
		loop_control_variable->moveBefore(&entry->front());

		// Used to zero the loop control variable in the zeroLCV function
		// before entering a loop.
		Constant *zero_32 = Constant::getNullValue(
			IntegerType::getInt32Ty(loopheader->getContext())
		);

		if (lhTerminator->isUnconditional())
			ret = transformUnconditionalLoop(
				loop, loopheader, lhTerminator, loop_control_variable, zero_32
			);
		else
			ret = transformConditionalLoop(
				loop, loopheader, lhTerminator, loop_control_variable, zero_32
			);

		loopheader->getParent()->print(errs());  //Debugging
		return ret;
	}

  };
}

char LoopControl::ID = 0;
static RegisterPass<LoopControl> Y("loopControl", "Loop control Pass");

// Publically exposed interface to pass.
const PassInfo *const llvm::LoopControlID = &Y;
