/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 * <p>
 * All rights reserved.
 * <p>
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.codegen.tam;

import mavlc.errors.InternalCompilerError;
import mavlc.syntax.AstNode;
import mavlc.syntax.expression.Compare.Comparison;
import mavlc.syntax.function.Function;
import mavlc.syntax.statement.Declaration;
import mtam.Image;
import mtam.Instruction;
import mtam.Primitive;
import mtam.Register;
import mtam.debug.DebugSymbolContainer;
import mtam.interpreter.Value;

import java.util.*;
import java.util.Map.Entry;

import static mtam.Opcode.*;
import static mtam.Register.*;

@SuppressWarnings({"UnusedReturnValue", "unused"})
public class TamAssembler {
	
	protected final ArrayList<Instruction> code = new ArrayList<>();
	
	protected int nextInstAddr = 0;
	
	protected final Map<String, Integer> constantPool = new HashMap<>();
	
	protected int nextConstIndex = 0;
	
	protected int nextOffset;
	
	
	public static final int trueConst = 1;
	public static final int falseConst = 0;
	
	public int getNextOffset() {
		return nextOffset;
	}
	
	public void setNextOffset(int nextOffset) {
		this.nextOffset = nextOffset;
	}
	
	public void resetNextOffset(int oldOffset) {
		if(this.nextOffset > oldOffset) {
			this.emitPop(0, this.nextOffset - oldOffset);
			this.nextOffset = oldOffset;
		}
	}
	
	protected final Instruction initialJump;
	
	public TamAssembler() {
		initialJump = new Instruction(CALL, CB, 0, -1);
		addInstruction(initialJump).addName("main");
		emitHaltInstruction();
	}
	
	public Image getImage() {
		Image image = new Image();
		List<String> constants = new ArrayList<>();
		
		List<Entry<String, Integer>> entries = new ArrayList<>(constantPool.entrySet());
		entries.sort(Comparator.comparingInt(Entry::getValue));
		for(int i = 0; i < entries.size(); i++) {
			Entry<String, Integer> entry = entries.get(i);
			if(entry.getValue() != i)
				throw new InternalCompilerError("Non consecutive string constant ids");
			constants.add(entry.getKey());
		}
		image.stringConstants = constants.toArray(new String[0]);
		image.instructions = code.toArray(new Instruction[0]);
		return image;
	}
	
	private final Stack<AstNode> context = new Stack<>();
	
	public void pushContext(AstNode node) {
		context.push(node);
	}
	
	public void popContext() {
		context.pop();
	}
	
	public final DebugSymbolContainer nextInstructionInfo = new DebugSymbolContainer();
	
	private Instruction addInstruction(Instruction inst) {
		if(!context.isEmpty()) {
			AstNode node = context.peek();
			inst.debugInfo.addLocation(node.sourceLocation);
		}
		inst.debugInfo.addAll(nextInstructionInfo);
		nextInstructionInfo.clear();
		code.add(inst);
		++nextInstAddr;
		return inst;
	}
	
	private int getStringIndex(String string) {
		if(constantPool.containsKey(string)) {
			return constantPool.get(string);
		} else {
			constantPool.put(string, nextConstIndex);
			return nextConstIndex++;
		}
	}
	
	protected final Map<Function, List<Instruction>> referencedFunctions = new HashMap<>();
	
	/**
	 * Add a function to the program code. If the added function is the main function,
	 * the initial jump is automatically patched.
	 *
	 * @param function Function to add to the code
	 */
	public void addNewFunction(Function function) {
		// the new function starts at the next instruction address.
		function.setCodeBaseOffset(nextInstAddr);
		
		// patch already emitted calls to this function.
		patchDeferredFunctionCalls(function);
		
		// if the new function is the main method, patch the initial jump to point to this function.
		if(function.name.equals("main")) initialJump.d = nextInstAddr;
		
		// function-local data starts at offset 2 relative to LB.
		// 0[LB] and 1[LB] are reserved for return address and dynamic link.
		nextOffset = 2;
		nextInstructionInfo
				.addComment(function.getSignature(), false)
				.addLabel(function.name);
	}
	
	/**
	 * Emit a Call to a Function.
	 *
	 * @param callee Function to call
	 * @return The generated instruction
	 */
	public Instruction emitFunctionCall(Function callee) {
		if(!callee.isCodeBaseOffsetSet()) {
			// code generation for the called function has not yet taken place,
			// store call instruction for later back-patch.
			Instruction call = new Instruction(CALL, CB, 0, -1);
			registerDeferredFunctionCall(callee, call);
			return addInstruction(call);
		}
		if(callee.getCodeBaseOffset() >= Primitive.baseAddress) {
			return addInstruction(new Instruction(CALL, PB, 0, callee.getCodeBaseOffset() - Primitive.baseAddress))
					.addName(callee.name);
		}
		return addInstruction(new Instruction(CALL, CB, 0, callee.getCodeBaseOffset()))
				.addName(callee.name);
	}
	
	private void registerDeferredFunctionCall(Function func, Instruction call) {
		if(!referencedFunctions.containsKey(func)) {
			referencedFunctions.put(func, new LinkedList<>());
		}
		referencedFunctions.get(func).add(call);
	}
	
	private void patchDeferredFunctionCalls(Function func) {
		if(referencedFunctions.containsKey(func)) {
			for(Instruction call : referencedFunctions.get(func)) {
				call.d = nextInstAddr;
			}
		}
	}
	
	/**
	 * Emit a return instruction.
	 *
	 * @param resultSize Size in words of the result
	 * @param argSize Size of the function arguments
	 * @return The generated instruction
	 */
	public Instruction emitReturn(int resultSize, int argSize) {
		return addInstruction(new Instruction(RETURN, null, resultSize, argSize));
	}
	
	/**
	 * Emit a halt instruction, which terminates the program execution.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitHaltInstruction() {
		return addInstruction(new Instruction(HALT, null, 0, 0));
	}
	
	/**
	 * Generates code that throws an error when the top stack value is smaller than lowerBound or greater than or equal to upperBound.
	 * Otherwise, the topmost stack element is left untouched.
	 *
	 * @param lowerBound Minimum value (inclusive)
	 * @param upperBound Maximum value (exclusive)
	 */
	public void emitBoundsCheck(int lowerBound, int upperBound) {
		// ..., value
		loadValue(Register.ST, 1, -1).addComment("start of bounds check [" + lowerBound + ", " + upperBound + ")", true);
		// ..., value, value
		loadIntegerValue(lowerBound);
		// ..., value, value, lower
		emitIntegerComparison(Comparison.LESS);
		// ..., value, bool
		Instruction jumpLt = emitConditionalJump(true, -1);
		
		// ..., value
		loadValue(Register.ST, 1, -1);
		// ..., value, value
		loadIntegerValue(upperBound);
		// ..., value, value, upper
		emitIntegerComparison(Comparison.GREATER_EQUAL);
		// ..., value, bool
		Instruction jumpGe = emitConditionalJump(true, -1);
		// ..., value
		
		Instruction jumpEnd = emitJump(-1);
		int fail = getNextInstructionAddress();
		backPatchJump(jumpLt, fail);
		backPatchJump(jumpGe, fail);
		loadStringValue("Index out of bounds");
		emitErr();
		
		int success = getNextInstructionAddress();
		backPatchJump(jumpEnd, success);
		nextInstructionInfo.addComment("end of bounds check [" + lowerBound + ", " + upperBound + ")", true);
	}
	
	/**
	 * Add a variable or constant to the local scope.
	 *
	 * @param decl Declared Entity to add
	 */
	public void addDeclaredEntity(Declaration decl) {
		decl.setLocalBaseOffset(nextOffset);
		nextOffset += decl.getType().wordSize;
	}
	
	/**
	 * Emit a load instruction, to load a single boolean value onto the stack.
	 *
	 * @param value Value of the boolean
	 * @return The generated instruction
	 */
	public Instruction loadBooleanValue(boolean value) {
		int literal = value ? trueConst : falseConst;
		return addInstruction(new Instruction(LOADL, null, 0, literal)).addType(Value.Type.boolT);
	}
	
	/**
	 * Emit a load instruction, to load a single integer value onto the stack.
	 *
	 * @param value Value of the int
	 * @return The generated instruction
	 */
	public Instruction loadIntegerValue(int value) {
		return addInstruction(new Instruction(LOADL, null, 0, value)).addType(Value.Type.intT);
	}
	
	/**
	 * Emit a load instruction, to load a single float value onto the stack.
	 *
	 * @param value Value of the float
	 * @return The generated instruction
	 */
	public Instruction loadFloatValue(float value) {
		return addInstruction(new Instruction(LOADL, null, 0, Float.floatToIntBits(value))).addType(Value.Type.floatT);
	}
	
	/**
	 * Emit a load instruction, which loads a string onto the stack.
	 *
	 * @param value Value of the StringValue.
	 * @return The generated instruction
	 */
	public Instruction loadStringValue(String value) {
		int literal = getStringIndex(value);
		return addInstruction(new Instruction(LOADL, null, 0, literal)).addType(Value.Type.stringT);
	}
	
	/**
	 * Emit a load instruction for an entity local to the function.
	 * Addressing on the stack will be relative to register LB.
	 *
	 * @param wordSize Number of words to load
	 * @param offset Offset from LB.
	 * @return The generated instruction.
	 */
	public Instruction loadLocalValue(int wordSize, int offset) {
		return addInstruction(new Instruction(LOAD, LB, wordSize, offset));
	}
	
	/**
	 * Emit a load instruction for an entity.
	 * Addressing on the stack will be relative to the given register.
	 *
	 * @param register Register used for address resolution
	 * @param wordSize Number of words to load
	 * @param offset Offset from the given Register
	 * @return The generated instruction
	 */
	public Instruction loadValue(Register register, int wordSize, int offset) {
		return addInstruction(new Instruction(LOAD, register, wordSize, offset));
	}
	
	/**
	 * Load an address relative to a register onto the stack.
	 * Loaded address equals register value + offset.
	 *
	 * @param register Base register
	 * @param offset Offset from the register
	 * @return The generated instruction
	 */
	public Instruction loadAddress(Register register, int offset) {
		return addInstruction(new Instruction(LOADA, register, 0, offset));
	}
	
	/**
	 * Emit a load instruction which uses the value on top
	 * of the stack as address and loads the given number of words.
	 *
	 * @param wordSize The number of words to load
	 * @return The generated instruction
	 */
	public Instruction loadFromStackAddress(int wordSize) {
		return addInstruction(new Instruction(LOADI, null, wordSize, 0));
	}
	
	/**
	 * Emit a store instruction which stores the given number
	 * of words to the address on top of the stack.
	 *
	 * @param wordSize The number of words to store
	 * @return The generated instruction
	 */
	public Instruction storeToStackAddress(int wordSize) {
		return addInstruction(new Instruction(STOREI, null, wordSize, 0));
	}
	
	/**
	 * Emit a store instruction for an entity local to the function.
	 * Addressing on the stack will be relative to register LB.
	 *
	 * @param wordSize Number of words to store
	 * @param offset Offset from LB
	 * @return The generated instruction
	 */
	public Instruction storeLocalValue(int wordSize, int offset) {
		return addInstruction(new Instruction(STORE, LB, wordSize, offset));
	}
	
	/**
	 * Emit a store instruction for an entity.
	 * Addressing on the stack will be relative to a given register.
	 *
	 * @param register Register
	 * @param wordSize Number of words to store
	 * @param offset Offset from given register
	 * @return The generated instruction
	 */
	public Instruction storeValue(Register register, int wordSize, int offset) {
		return addInstruction(new Instruction(STORE, register, wordSize, offset));
	}
	
	/**
	 * Emit a pop instruction. The pop instruction pops a result of size
	 * resultSize from the stack, deletes another popSize words from the
	 * stack and pushes the result back on the stack.
	 *
	 * @param resultSize Size of the result
	 * @param popSize Number of words to delete
	 * @return The generated instruction
	 */
	public Instruction emitPop(int resultSize, int popSize) {
		return addInstruction(new Instruction(POP, null, resultSize, popSize));
	}
	
	/**
	 * Emit a push instruction pushing size uninitialized words on the stack.
	 *
	 * @param size Number of words to push
	 * @return The generated instruction
	 */
	public Instruction emitPush(int size) {
		return addInstruction(new Instruction(PUSH, null, 0, size));
	}
	
	/**
	 * Emit an unconditional jump to the given code address.
	 *
	 * @param address The target code address
	 * @return The generated instruction
	 */
	public Instruction emitJump(int address) {
		return addInstruction(new Instruction(JUMP, CB, 0, address));
	}
	
	/**
	 * Emit a conditional jump to the given code address under the given condition.
	 *
	 * @param condition The condition of the jump
	 * @param address The target code address
	 * @return The generated instruction
	 */
	public Instruction emitConditionalJump(boolean condition, int address) {
		return emitConditionalJump(condition ? trueConst : falseConst, address);
	}
	
	/**
	 * Emit a conditional jump to the given code address under the given condition.
	 *
	 * @param condition The condition of the jump
	 * @param address The target code address
	 * @return The generated instruction
	 */
	public Instruction emitConditionalJump(int condition, int address) {
		return addInstruction(new Instruction(JUMPIF, CB, condition, address));
	}
	
	/**
	 * Get the code address of the next instruction emitted.
	 *
	 * @return The next free code address
	 */
	public int getNextInstructionAddress() {
		return nextInstAddr;
	}
	
	/**
	 * Patch a jump-instruction to point to a new address.
	 *
	 * @param inst The jump instruction to be modified
	 * @param newAddress The new address
	 */
	public void backPatchJump(Instruction inst, int newAddress) {
		if(!(inst.op == JUMPIF || inst.op == JUMP)) {
			throw new UnsupportedOperationException("Can only backpatch jump instruction!");
		}
		inst.d = newAddress;
	}
	
	/**
	 * Emits a call to the {@link mtam.Primitive#err} primitive, which consumes a string id to be used as error message.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitErr() {
		return callPrimitive(Primitive.err);
	}
	
	/**
	 * Emits an Integer Negation, which consumes the topmost Element of the stack, and pushes
	 * its Negation onto the stack.
	 * The consumed Element is interpreted as Integer Number.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitIntegerNegation() {
		return callPrimitive(Primitive.negI);
	}
	
	/**
	 * Emits an Integer Multiplication, which consumes the two topmost Elements of the stack,
	 * multiplies them and pushed the result onto the stack.
	 * The consumed Elements are interpreted as Integer Numbers.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitIntegerMultiplication() {
		return callPrimitive(Primitive.mulI);
	}
	
	/**
	 * Emits an Integer Division, which consumes the two topmost Elements of the stack, divides the
	 * lower one by the upper one, and pushes the result onto the stack.
	 * The consumed Elements are interpreted as Integer Numbers.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitIntegerDivision() {
		return callPrimitive(Primitive.divI);
	}
	
	/**
	 * Emits an Integer Addition, which consumes the two topmost Elements of the stack, adds them up
	 * and pushes the result onto the stack.
	 * The consumed Elements are interpreted as Integer Numbers.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitIntegerAddition() {
		return callPrimitive(Primitive.addI);
	}
	
	/**
	 * Emits an Integer Subtraction, which consumes the two topmost Elements of the stack, subtracts
	 * the upper one from the lower one and pushes the result to the stack.
	 * The consumed Elements are interpreted as Integer Numbers.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitIntegerSubtraction() {
		return callPrimitive(Primitive.subI);
	}
	
	/**
	 * Emits an Integer Exponentiation, which consumes the two topmost Elements of the stack.
	 * The topmost is interpreted as the power, the one below as base.
	 * The result is than pushed onto the stack.
	 * The consumed Elements are interpreted as Integer Numbers.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitIntegerExponentiation() {
		return callPrimitive(Primitive.powInt);
	}
	
	/**
	 * Emits an Integer Comparison Operation, which consumes the two topmost Elements of the stack,
	 * and pushes the Comparison Result onto the stack.
	 * The consumed Elements are interpreted as Integer Numbers.
	 *
	 * @param comparison Comparison Operator
	 * @return The generated instruction
	 */
	public Instruction emitIntegerComparison(Comparison comparison) {
		switch(comparison) {
			case EQUAL:
				return callPrimitive(Primitive.eqI);
			case NOT_EQUAL:
				return callPrimitive(Primitive.neI);
			case LESS:
				return callPrimitive(Primitive.ltI);
			case LESS_EQUAL:
				return callPrimitive(Primitive.leI);
			case GREATER:
				return callPrimitive(Primitive.gtI);
			case GREATER_EQUAL:
				return callPrimitive(Primitive.geI);
			default:
				throw new InternalCompilerError("Invalid comparison operator: " + comparison);
		}
	}
	
	/**
	 * Emits a Floating Point Negation, which consumes the topmost Element of the stack, and pushes
	 * its Negation onto the stack.
	 * The consumed Elements are interpreted as Floating Point Numbers.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitFloatNegation() {
		return callPrimitive(Primitive.negF);
	}
	
	/**
	 * Emits a Floating Point Multiplication, which consumes the two topmost Elements of the stack,
	 * multiplies them and pushes the result onto the stack.
	 * The consumed Elements are interpreted as Floating Point Numbers.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitFloatMultiplication() {
		return callPrimitive(Primitive.mulF);
	}
	
	/**
	 * Emits a Floating Point Division, which consumes the two topmost Elements of the stack, divides
	 * the lower one by the upper one and pushes the result onto the stack.
	 * The consumed Elements are interpreted as Floating Point Numbers.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitFloatDivision() {
		return callPrimitive(Primitive.divF);
	}
	
	/**
	 * Emits a Floating Point Addition, which consumes the two topmost Elements of the stack, adds them
	 * up and pushes the result onto the stack.
	 * The consumed Elements are interpreted as Floating Point Numbers.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitFloatAddition() {
		return callPrimitive(Primitive.addF);
	}
	
	/**
	 * Emits a Floating Point Subtraction, which consumes the two topmost Elements of the stack, subtracts
	 * the upper one from the lower one and pushes the result onto the stack.
	 * The consumed Elements are interpreted as Floating Point Numbers.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitFloatSubtraction() {
		return callPrimitive(Primitive.subF);
	}
	
	/**
	 * Emits a Float Exponentiation, which consumes the two topmost Elements of the stack.
	 * The topmost is interpreted as the power, the one below as base.
	 * The result is than pushed onto the stack.
	 * The consumed Elements are interpreted as Float Numbers.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitFloatExponentiation() {
		return callPrimitive(Primitive.powFloat);
	}
	
	/**
	 * Emits a Floating Point Comparison Operation, that consumes the two topmost stack elements
	 * and pushes the Comparison result onto the stack.
	 * The consumed Elements are interpreted as Floating Point Numbers.
	 *
	 * @param comparison Comparision Operator
	 * @return The generated instruction
	 */
	public Instruction emitFloatComparison(Comparison comparison) {
		switch(comparison) {
			case EQUAL:
				return callPrimitive(Primitive.eqF);
			case NOT_EQUAL:
				return callPrimitive(Primitive.neF);
			case LESS:
				return callPrimitive(Primitive.ltF);
			case LESS_EQUAL:
				return callPrimitive(Primitive.leF);
			case GREATER:
				return callPrimitive(Primitive.gtF);
			case GREATER_EQUAL:
				return callPrimitive(Primitive.geF);
			default:
				throw new InternalCompilerError("Invalid comparison operator: " + comparison);
		}
	}
	
	/**
	 * Emit a Call to the Matrix Multiplication Primitive for ints.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitIntegerMatrixMultiplication() {
		return callPrimitive(Primitive.matMulI);
	}
	
	/**
	 * Emit a Call to the Matrix Multiplication Primitive for floats.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitFloatMatrixMultiplication() {
		return callPrimitive(Primitive.matMulF);
	}
	
	/**
	 * Emit a call to the matrix transposition primitive.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitMatrixTranspose() {
		return callPrimitive(Primitive.matTranspose);
	}
	
	/**
	 * Emit a logical Not Operation that consumes the topmost element of the stack, and replaces it with
	 * its negation.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitLogicalNot() {
		return callPrimitive(Primitive.not);
	}
	
	/**
	 * Emit a logical Or Operation that consumes the two topmost stack elements, applies the Logical
	 * Or Operation, and pushes the result to the stack.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitLogicalOr() {
		return callPrimitive(Primitive.or);
	}
	
	/**
	 * Emit a logical And Operation that consumes the two topmost stack elements, applies the Logical
	 * And Operation, and pushes the result to the stack.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitLogicalAnd() {
		return callPrimitive(Primitive.and);
	}
	
	/**
	 * Emit a call to the increment (succ) primitive.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitIncrement() {
		return callPrimitive(Primitive.succ);
	}
	
	/**
	 * Emit a call to the decrement (pred) primitive.
	 *
	 * @return The generated instruction
	 */
	public Instruction emitDecrement() {
		return callPrimitive(Primitive.pred);
	}
	
	/**
	 * Emits a call to a primitive function.
	 *
	 * @param primitive The primitive to call
	 * @return The generated instruction
	 */
	public Instruction callPrimitive(Primitive primitive) {
		return addInstruction(new Instruction(CALL, PB, 0, primitive.displacement));
	}
	
	/**
	 * Returns the program code as List of Instructions.
	 *
	 * @return the code
	 */
	public ArrayList<Instruction> getCode() {
		return code;
	}
	
	/**
	 * Returns the constant pool, which contains all constants.
	 *
	 * @return the constantPool
	 */
	public Map<Integer, String> getConstantPool() {
		Map<Integer, String> cp = new HashMap<>();
		for(Entry<String, Integer> constant : constantPool.entrySet()) {
			cp.put(constant.getValue(), constant.getKey());
		}
		return cp;
	}
}
