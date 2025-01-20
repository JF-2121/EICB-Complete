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
package mtam.interpreter;

import mtam.Primitive;
import mtam.Register;
import mtam.errors.ErrorCode;
import mtam.errors.ExecutionException;

import static mtam.Register.*;

public class MachineState {
	public static final int maxMemorySize = 1 << 20;
	public static final int maxCodeMemSize = 1 << 15;
	public static final int maxInstructions = maxCodeMemSize - Primitive.count;
	
	// general state
	public ExecutionState executionState = ExecutionState.running;
	public final Value[] registers = new Value[Register.count];
	public final Value[] memory = new Value[maxMemorySize];
	
	// error reporting
	public ErrorCode errorCode;
	public String errorMessage;
	public int errorLocation;
	
	final Profiler stats;
	
	public MachineState(Profiler stats) {
		this.stats = stats;
	}
	
	/**
	 * @param reg The register to read
	 * @return Contents of register {@code reg}
	 */
	public Value getReg(Register reg) {
		stats.regRead(reg);
		Value val = registers[reg.id];
		return (val != null) ? val : Value.zero;
	}
	
	public int getRegI(Register reg) {
		return getReg(reg).asInt();
	}
	
	public float getRegF(Register reg) {
		return getReg(reg).asFloat();
	}
	
	/**
	 * @param reg The register to write
	 * @param val Value to overwrite the register contents with
	 */
	public void setReg(Register reg, Value val) {
		stats.regWrite(reg);
		registers[reg.id] = val;
	}
	
	public void setReg(Register reg, int val) {
		setReg(reg, new Value(val));
	}
	
	public void setReg(Register reg, float val) {
		setReg(reg, new Value(val));
	}
	
	/**
	 * @param address The memory address to read from
	 * @return The value at that memory address
	 * @throws ExecutionException If the address is invalid
	 */
	public Value getMem(int address) throws ExecutionException {
		stats.memRead();
		if(address < 0 || address >= memory.length)
			throw new ExecutionException(ErrorCode.invalidAddress, "Read at invalid memory address", getRegI(CP));
		return memory[address] != null ? memory[address] : Value.zero;
	}
	
	public int getMemI(int address) throws ExecutionException {
		return getMem(address).asInt();
	}
	
	public float getMemF(int address) throws ExecutionException {
		return getMem(address).asFloat();
	}
	
	/**
	 * @param address The memory address to read from
	 * @param val The value to write to that memory address
	 * @throws ExecutionException If the address is invalid
	 */
	public void setMem(int address, Value val) throws ExecutionException {
		stats.memWrite();
		if(address < 0 || address >= memory.length)
			throw new ExecutionException(ErrorCode.invalidAddress, "Write at invalid memory address", getRegI(CP));
		memory[address] = val;
	}
	
	public void setMem(int address, int val) throws ExecutionException {
		setMem(address, new Value(val));
	}
	
	public void setMem(int address, float val) throws ExecutionException {
		setMem(address, new Value(val));
	}
	
	/**
	 * @param src The memory address to read from
	 * @param dst The memory address to write to
	 * @param count The number of words to copy
	 * @throws ExecutionException If any read or write occurs at an invalid address
	 */
	public void copyMem(int src, int dst, int count) throws ExecutionException {
		stats.memCopy();
		for(int i = 0; i < count; i++) {
			setMem(dst + i, getMem(src + i));
		}
	}
	
	/**
	 * @param dst The memory address to write to
	 * @param count The number of words to copy
	 * @throws ExecutionException If any read or write occurs at an invalid address
	 */
	public void zeroMem(int dst, int count, Value.Type type) throws ExecutionException {
		stats.memZero();
		Value value = new Value(type, 0);
		for(int i = 0; i < count; i++) {
			setMem(dst + i, value);
		}
	}
	
	public void raiseError(ExecutionException e) {
		raiseError(e.errorCode, e.getMessage(), e.getErrorLocation().orElse(-1));
	}
	
	public void raiseError(ErrorCode code, String message, int location) {
		executionState = ExecutionState.error;
		errorCode = code;
		errorMessage = message;
		errorLocation = location;
	}
	
	/**
	 * Increments the instruction pointer by one.
	 */
	public void incCP() {
		Value cp = getReg(CP);
		setReg(CP, new Value(Value.Type.cAddrT, cp.bits + 1));
	}
	
	/**
	 * Increases the stack size by {@code n} words.
	 *
	 * @param n Number of words to increment by
	 * @return The PREVIOUS contents of the ST register
	 * @throws ExecutionException In case of a stack overflow
	 */
	public int incStack(int n) throws ExecutionException {
		int st = getRegI(ST);
		if(st + n >= memory.length)
			throw new ExecutionException(ErrorCode.stackOverflow, "Stack overflow", registers[CP.id].asInt());
		setReg(ST, st + n);
		return st;
	}
	
	/**
	 * Decreases the stack size by {@code n} words.
	 *
	 * @param n Number of words to decrement by
	 * @return The NEW contents of the ST register
	 * @throws ExecutionException In case of a stack underflow
	 */
	public int decStack(int n) throws ExecutionException {
		int st = getRegI(ST) - n;
		if(st < registers[SB.id].asInt())
			throw new ExecutionException(ErrorCode.stackUnderflow, "Stack underflow", registers[CP.id].asInt());
		setReg(ST, st);
		return st;
	}
	
	/**
	 * Pushes a single word onto the stack.
	 *
	 * @param val Value to push onto the stack
	 * @throws ExecutionException In case of a stack overflow
	 */
	public void pushStack(Value val) throws ExecutionException {
		int st = getRegI(ST);
		if(st + 1 >= memory.length)
			throw new ExecutionException(ErrorCode.stackOverflow, "Stack overflow", registers[CP.id].asInt());
		setMem(st, val);
		setReg(ST, st + 1);
	}
	
	/**
	 * Pops a single value from the stack.
	 *
	 * @return The popped value
	 * @throws ExecutionException In case of a stack underflow
	 */
	public Value popStack() throws ExecutionException {
		int st = getRegI(ST) - 1;
		if(st < registers[SB.id].asInt())
			throw new ExecutionException(ErrorCode.stackUnderflow, "Stack underflow", registers[CP.id].asInt());
		Value val = getMem(st);
		setReg(ST, st);
		return val;
	}
	
	public void throwIfError() throws ExecutionException {
		if(executionState == ExecutionState.error)
			throw new ExecutionException(errorCode, errorMessage, errorLocation);
	}
}
