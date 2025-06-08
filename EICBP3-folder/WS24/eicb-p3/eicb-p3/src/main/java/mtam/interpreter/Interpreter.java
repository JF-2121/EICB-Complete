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

import mtam.Image;
import mtam.Instruction;
import mtam.Primitive;
import mtam.Register;
import mtam.errors.ErrorCode;
import mtam.errors.ExecutionException;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static mtam.Register.*;

@SuppressWarnings({"unused", "UnusedReturnValue"})
public class Interpreter {
	
	public Path basePath;
	public final InputStream input;
	public final PrintStream output;
	public final Scanner scanner;
	
	public MachineState state;
	public Profiler stats;
	public Image image;
	
	public int cycles;
	
	public Interpreter(InputStream input, PrintStream output) {
		this.input = input;
		this.output = output;
		this.scanner = new Scanner(input);
		reset(false);
	}
	
	public boolean loadImage(Path path) {
		try {
			reset(true);
			Image image = new Image(path);
			image.load();
			loadImage(image);
			return true;
		} catch(IOException e) {
			state.raiseError(ErrorCode.ioError, "Failed to load program image: " + e.getLocalizedMessage(), -1);
			return false;
		} catch(ExecutionException e) {
			state.raiseError(e);
			return false;
		}
	}
	
	public void loadImage(Image image) {
		reset(true);
		this.image = image;
		init();
	}
	
	public void reset(boolean unloadImage) {
		if(unloadImage) image = null;
		stats = new Profiler();
		state = new MachineState(stats);
		cycles = 0;
		init();
	}
	
	public void init() {
		if(image == null) return;
		state.registers[CB.id] = new Value(Value.Type.cAddrT, 0);
		state.registers[CT.id] = new Value(image.instructions.length);
		state.registers[PB.id] = new Value(MachineState.maxInstructions);
		state.registers[PT.id] = new Value(MachineState.maxCodeMemSize);
		state.registers[SB.id] = new Value(Value.Type.sAddrT, 0);
		state.registers[ST.id] = new Value(Value.Type.sAddrT, 0);
		state.registers[LB.id] = new Value(Value.Type.sAddrT, 0);
		state.registers[CP.id] = new Value(Value.Type.cAddrT, 0);
	}
	
	public void run() {
		run(0);
	}
	
	public void run(int maxCycles) {
		while(state.executionState == ExecutionState.running && (maxCycles <= 0 || cycles < maxCycles)) {
			advance();
		}
		if(state.executionState == ExecutionState.running) {
			System.out.println("Aborted execution after " + maxCycles + " cycles");
		} else {
			printStatus();
		}
	}
	
	public void printStatus() {
		output.println();
		switch(state.executionState) {
			case running:
				output.println("The program is running.");
				break;
			case halted:
				output.println("The program has finished successfully.");
				break;
			case terminated:
				output.println("The program was terminated by the user.");
				break;
			case error:
				output.println("The program has finished with an error: " + state.errorCode);
				output.println("Error message: " + state.errorMessage);
				break;
		}
	}
	
	public boolean advance() {
		if(state.executionState != ExecutionState.running) return false;

		int instAddr = 0;
		try {
			// fetch instruction
			instAddr = state.getRegI(CP);
			Instruction inst = image.getInstruction(instAddr);
			// execute instruction
			return execute(inst);
			
		} catch(ExecutionException e) {
			// Fill in probable location if it's not known yet
			if (e.getErrorLocation().isEmpty()) {
				e.setErrorLocation(instAddr);
			}
			state.raiseError(e);
			return false;
			
		} finally {
			cycles++;
		}
	}
	
	protected int getCurrentInstructionAddress() {
		return state.registers[CP.id].asInt();
	}
	
	protected Instruction getCurrentInstruction() throws ExecutionException {
		return image.getInstruction(getCurrentInstructionAddress());
	}
	
	public boolean execute(Instruction inst) throws ExecutionException {
		if(inst == null) {
			state.raiseError(ErrorCode.internalError, "Null instruction", state.getRegI(CP));
			return false;
		}
		if(inst.op == null) {
			state.raiseError(ErrorCode.internalError, "Null opcode", state.getRegI(CP));
			return false;
		}
		
		stats.exec(inst.op);
		
		// actually execute the instruction
		executors[inst.op.id].execute(inst.r, inst.n, inst.d);
		
		return true;
	}
	
	public void callPrimitive(int address) throws ExecutionException {
		Primitive prim = Primitive.fromDisplacement(address - state.registers[PB.id].bits);
		stats.exec(prim);
		primitives[prim.displacement].call();
	}
	
	private int address(Register base, int offset) {
		return state.getRegI(base) + offset;
	}
	
	private Value.Type regType(Register r) {
		return Register.codeRegisters.contains(r) ? Value.Type.cAddrT : Register.stackRegisters.contains(r) ? Value.Type.sAddrT : Value.Type.unknown;
	}
	
	////////////////////
	/// INSTRUCTIONS ///
	////////////////////
	
	protected interface InstExecutor {
		void execute(Register r, int n, int d) throws ExecutionException;
	}
	
	protected final InstExecutor[] executors = new InstExecutor[]{
			this::executeLoad,
			this::executeLoadA,
			this::executeLoadI,
			this::executeLoadL,
			this::executeStore,
			this::executeStoreI,
			this::executeCall,
			this::executeCallI,
			this::executeReturn,
			this::executePush,
			this::executePop,
			this::executeJump,
			this::executeJumpI,
			this::executeJumpIf,
			this::executeHalt,
	};
	
	/**
	 * Copies n words from d[r] to the stack and increases it accordingly.<br>
	 * Stack transition: {@code ... -> ..., [n]}
	 *
	 * @param r Base register for source address resolution
	 * @param n Number of words to copy
	 * @param d Offset for source address resolution
	 * @throws ExecutionException In case of an invalid address or stack overflow
	 */
	public void executeLoad(Register r, int n, int d) throws ExecutionException {
		int src = address(r, d);
		int dst = state.incStack(n);
		state.copyMem(src, dst, n);
		state.incCP();
	}
	
	/**
	 * Pushes the address d[r] to the stack.<br>
	 * Stack transition: {@code ... -> ..., addr}
	 *
	 * @param r Base register for source address resolution
	 * @param n -
	 * @param d Offset for source address resolution
	 * @throws ExecutionException In case of an invalid address or stack overflow
	 */
	public void executeLoadA(Register r, int n, int d) throws ExecutionException {
		state.pushStack(new Value(regType(r), address(r, d)));
		state.incCP();
	}
	
	/**
	 * Pops an address from the stack and pushes n words from that address to the stack.<br>
	 * Stack transition: {@code ..., addr -> ..., [n]}
	 *
	 * @param r -
	 * @param n Number of words to copy
	 * @param d -
	 * @throws ExecutionException In case of an invalid address or stack over- or underflow
	 */
	public void executeLoadI(Register r, int n, int d) throws ExecutionException {
		int src = state.popStack().asInt();
		int dst = state.incStack(n);
		state.copyMem(src, dst, n);
		state.incCP();
	}
	
	/**
	 * Pushes a literal value to the stack.<br>
	 * Stack transition: {@code ... -> ..., d}
	 *
	 * @param r -
	 * @param n -
	 * @param d The literal value to push
	 * @throws ExecutionException In case of an invalid address or stack overflow
	 */
	public void executeLoadL(Register r, int n, int d) throws ExecutionException {
		Value.Type type = getCurrentInstruction().debugInfo.getType();
		state.pushStack(new Value(type, d));
		state.incCP();
	}
	
	/**
	 * Stores n words from the stack to memory at address d[r] and decreases the stack accordingly.<br>
	 * Stack transition: {@code ..., [n] -> ...}
	 *
	 * @param r Base register for destination address resolution
	 * @param n Number of words to copy
	 * @param d Offset for destination address resolution
	 * @throws ExecutionException In case of an invalid address or stack underflow
	 */
	public void executeStore(Register r, int n, int d) throws ExecutionException {
		int dst = address(r, d);
		int src = state.decStack(n);
		state.copyMem(src, dst, n);
		state.incCP();
	}
	
	/**
	 * Pops an address from the stack and stores n words to memory at that address.<br>
	 * Stack transition: {@code ..., [n], addr -> ...}
	 *
	 * @param r -
	 * @param n Number of words to copy
	 * @param d -
	 * @throws ExecutionException In case of an invalid address or stack underflow
	 */
	public void executeStoreI(Register r, int n, int d) throws ExecutionException {
		int dst = state.popStack().asInt();
		int src = state.decStack(n);
		state.copyMem(src, dst, n);
		state.incCP();
	}
	
	/**
	 * Creates a new stack frame and jumps to address d[r].<br>
	 * Stack transition: {@code ... -> ..., dynLink, retAddr}
	 *
	 * @param r Base register for jump address resolution
	 * @param n -
	 * @param d Offset for jump address resolution
	 * @throws ExecutionException In case of an invalid address or stack overflow
	 */
	public void executeCall(Register r, int n, int d) throws ExecutionException {
		performCall(address(r, d));
	}
	
	/**
	 * Pops an address from the stack, then creates a new stack frame and jumps to that address.<br>
	 * Stack transition: {@code ..., addr -> ..., dynLink, retAddr}
	 *
	 * @param r -
	 * @param n -
	 * @param d -
	 * @throws ExecutionException In case of an invalid address or stack over- or underflow
	 */
	public void executeCallI(Register r, int n, int d) throws ExecutionException {
		performCall(state.popStack().asInt());
	}
	
	private void performCall(int addr) throws ExecutionException {
		if(addr >= state.getRegI(PB)) {
			callPrimitive(addr);
			state.incCP();
		} else {
			int st = state.incStack(2);
			Value cp = state.getReg(CP);
			state.setMem(st, state.getReg(LB)); // store local base register
			state.setMem(st + 1, new Value(Value.Type.cAddrT, cp.bits + 1)); // store return address
			state.setReg(LB, st);
			state.setReg(ST, st + 2);
			state.setReg(CP, new Value(Value.Type.cAddrT, addr));
		}
	}
	
	/**
	 * Pops a return value of n words from the stack, destroys the current stack frame, pops d words of parameters and
	 * finally pushes the return value back onto the stack.<br>
	 * Stack transition: {@code ..., [d], dynLink, retAddr, [n] -> ..., [n]}
	 *
	 * @param r -
	 * @param n Size of the return value in words
	 * @param d Size of the method parameters in words
	 * @throws ExecutionException In case of an invalid address or stack underflow
	 */
	public void executeReturn(Register r, int n, int d) throws ExecutionException {
		int lb = state.getRegI(LB);
		int st = state.getRegI(ST);
		Value dynLink = state.getMem(lb);
		Value retAddr = state.getMem(lb + 1);
		
		// copy return value
		int src = st - n;
		int dst = lb - d;
		state.copyMem(src, dst, n);
		
		// restore registers
		state.setReg(ST, dst + n);
		state.setReg(LB, dynLink);
		state.setReg(CP, retAddr);
	}
	
	/**
	 * Pushes d words onto the stack, initialized with zeroes.<br>
	 * Stack transition: {@code ..., -> ..., [d]}
	 *
	 * @param r -
	 * @param n -
	 * @param d Number of words to push
	 * @throws ExecutionException In case of a stack overflow
	 */
	public void executePush(Register r, int n, int d) throws ExecutionException {
		Value.Type type = getCurrentInstruction().debugInfo.getType();
		int st = state.incStack(d);
		state.zeroMem(st, d, type);
		state.incCP();
	}
	
	/**
	 * Pops n words from the stack, then d more words and finally pushes the n words from before back onto the stack.<br>
	 * Stack transition: {@code ..., [d], [n] -> ..., [n]}
	 *
	 * @param r -
	 * @param n Number of words to save
	 * @param d Number of words to delete
	 * @throws ExecutionException In case of a stack overflow
	 */
	public void executePop(Register r, int n, int d) throws ExecutionException {
		int st = state.getRegI(ST);
		int src = st - n;
		int dst = st - n - d;
		state.copyMem(src, dst, n);
		state.setReg(ST, dst + n);
		state.incCP();
	}
	
	/**
	 * Jumps to the address d[r].<br>
	 * Stack transition: {@code ... -> ...}
	 *
	 * @param r Base register for jump address resolution
	 * @param n -
	 * @param d Offset for jump address resolution
	 */
	public void executeJump(Register r, int n, int d) {
		state.setReg(CP, new Value(Value.Type.cAddrT, address(r, d)));
	}
	
	/**
	 * Pops an address from the stack and jumps to it.<br>
	 * Stack transition: {@code ..., addr -> ...}
	 *
	 * @param r -
	 * @param n -
	 * @param d -
	 * @throws ExecutionException In case of an invalid address
	 */
	public void executeJumpI(Register r, int n, int d) throws ExecutionException {
		state.setReg(CP, new Value(Value.Type.cAddrT, popI()));
	}
	
	/**
	 * Pops a value from the stack and jumps to d[r], if the value equals n.<br>
	 * Stack transition: {@code ..., cond -> ...}
	 *
	 * @param r Base register for jump address resolution
	 * @param n The condition to compare to
	 * @param d Offset for jump address resolution
	 * @throws ExecutionException In case of a stack overflow
	 */
	public void executeJumpIf(Register r, int n, int d) throws ExecutionException {
		int value = state.popStack().asInt();
		if(value == n)
			state.setReg(CP, new Value(Value.Type.cAddrT, address(r, d)));
		else
			state.incCP();
	}
	
	/**
	 * Stops the interpreter.<br>
	 * Stack transition: {@code ... -> ...}
	 *
	 * @param r -
	 * @param n -
	 * @param d -
	 */
	public void executeHalt(Register r, int n, int d) {
		state.executionState = ExecutionState.halted;
	}
	
	//////////////////
	/// PRIMITIVES ///
	//////////////////
	
	protected interface PrimExecutor {
		void call() throws ExecutionException;
	}
	
	protected final PrimExecutor[] primitives = new PrimExecutor[]{
			this::callNop,
			this::callErr,
			
			this::callNot,
			this::callAnd,
			this::callOr,
			
			this::callSucc,
			this::callPred,
			
			this::callNegI,
			this::callAddI,
			this::callSubI,
			this::callMulI,
			this::callDivI,
			this::callModI,
			
			this::callEqI,
			this::callNeI,
			this::callLtI,
			this::callLeI,
			this::callGtI,
			this::callGeI,
			
			this::callNegF,
			this::callAddF,
			this::callSubF,
			this::callMulF,
			this::callDivF,
			
			this::callEqF,
			this::callNeF,
			this::callLtF,
			this::callLeF,
			this::callGtF,
			this::callGeF,
			
			this::callReadImage,
			this::callWriteImage,
			
			this::callReadIntMatrix64,
			this::callReadIntMatrix16,
			this::callReadIntMatrix9,
			this::callWriteIntMatrix64,
			this::callWriteIntMatrix16,
			this::callWriteIntMatrix9,
			
			this::callReadFloatMatrix64,
			this::callReadFloatMatrix16,
			this::callReadFloatMatrix9,
			this::callWriteFloatMatrix64,
			this::callWriteFloatMatrix16,
			this::callWriteFloatMatrix9,
			
			this::callPowInt,
			this::callPowFloat,
			this::callSqrtInt,
			this::callSqrtFloat,
			
			this::callPrintInt,
			this::callPrintFloat,
			this::callPrintBool,
			this::callPrintString,
			this::callPrintLine,
			
			this::callReadInt,
			this::callReadFloat,
			this::callReadBool,
			
			this::callInt2float,
			this::callFloat2int,
			
			this::callMatMulI,
			this::callMatMulF,
			this::callMatTranspose,
	};
	
	public void callNop() {
		// do nothing
	}
	
	public void callErr() throws ExecutionException {
		throw new ExecutionException(ErrorCode.runtimeError, image.getStringConstant(popI()), state.getRegI(CP));
	}
	
	public void callNot() throws ExecutionException {
		boolean v = popB();
		push(!v);
	}
	
	public void callAnd() throws ExecutionException {
		boolean r = popB();
		boolean l = popB();
		push(l & r);
	}
	
	public void callOr() throws ExecutionException {
		boolean r = popB();
		boolean l = popB();
		push(l | r);
	}
	
	public void callSucc() throws ExecutionException {
		Value val = pop();
		push(new Value(val.type, val.bits + 1));
	}
	
	public void callPred() throws ExecutionException {
		Value val = pop();
		push(new Value(val.type, val.bits - 1));
	}
	
	public void callNegI() throws ExecutionException {
		int v = popI();
		push(-v);
	}
	
	public void callAddI() throws ExecutionException {
		Value r = pop();
		Value l = pop();
		int sum = l.bits + r.bits;
		if((l.type == Value.Type.cAddrT || l.type == Value.Type.sAddrT) && r.type == Value.Type.intT)
			push(new Value(l.type, sum));
		else if((r.type == Value.Type.cAddrT || r.type == Value.Type.sAddrT) && l.type == Value.Type.intT)
			push(new Value(r.type, sum));
		else
			push(sum);
	}
	
	public void callSubI() throws ExecutionException {
		Value r = pop();
		Value l = pop();
		push(new Value(l.type, l.bits - r.bits));
	}
	
	public void callMulI() throws ExecutionException {
		int r = popI();
		int l = popI();
		push(l * r);
	}
	
	public void callDivI() throws ExecutionException {
		int r = popI();
		int l = popI();
		if(r == 0)
			throw new ExecutionException(ErrorCode.zeroDivision, "Divided by zero", state.getRegI(CP));
		push(l / r);
	}
	
	public void callModI() throws ExecutionException {
		int r = popI();
		int l = popI();
		push(l % r);
	}
	
	public void callEqI() throws ExecutionException {
		int r = popI();
		int l = popI();
		push(l == r);
	}
	
	public void callNeI() throws ExecutionException {
		int r = popI();
		int l = popI();
		push(l != r);
	}
	
	public void callLtI() throws ExecutionException {
		int r = popI();
		int l = popI();
		push(l < r);
	}
	
	public void callLeI() throws ExecutionException {
		int r = popI();
		int l = popI();
		push(l <= r);
	}
	
	public void callGtI() throws ExecutionException {
		int r = popI();
		int l = popI();
		push(l > r);
	}
	
	public void callGeI() throws ExecutionException {
		int r = popI();
		int l = popI();
		push(l >= r);
	}
	
	public void callNegF() throws ExecutionException {
		float v = popF();
		push(-v);
	}
	
	public void callAddF() throws ExecutionException {
		float r = popF();
		float l = popF();
		push(l + r);
	}
	
	public void callSubF() throws ExecutionException {
		float r = popF();
		float l = popF();
		push(l - r);
	}
	
	public void callMulF() throws ExecutionException {
		float r = popF();
		float l = popF();
		push(l * r);
	}
	
	public void callDivF() throws ExecutionException {
		float r = popF();
		float l = popF();
		if(r == 0f)
			throw new ExecutionException(ErrorCode.zeroDivision, "Divided by zero", state.getRegI(CP));
		push(l / r);
	}
	
	public void callEqF() throws ExecutionException {
		float r = popF();
		float l = popF();
		push(l == r);
	}
	
	public void callNeF() throws ExecutionException {
		float r = popF();
		float l = popF();
		push(l != r);
	}
	
	public void callLtF() throws ExecutionException {
		float r = popF();
		float l = popF();
		push(l < r);
	}
	
	public void callLeF() throws ExecutionException {
		float r = popF();
		float l = popF();
		push(l <= r);
	}
	
	public void callGtF() throws ExecutionException {
		float r = popF();
		float l = popF();
		push(l > r);
	}
	
	public void callGeF() throws ExecutionException {
		float r = popF();
		float l = popF();
		push(l >= r);
	}
	
	private Path resolvePath(String relativePath) {
		if(relativePath == null || relativePath.trim().isEmpty()) return null;
		return basePath == null ? Paths.get(relativePath).toAbsolutePath() : basePath.resolve(relativePath).toAbsolutePath();
	}
	
	private void performIntMatrixRead(int rows, int cols) throws ExecutionException {
		performMatrixRead(rows, cols, str -> new Value(Integer.parseInt(str)));
	}
	
	private void performFloatMatrixRead(int rows, int cols) throws ExecutionException {
		performMatrixRead(rows, cols, str -> new Value(Float.parseFloat(str)));
	}
	
	private void performMatrixRead(int rows, int cols, Function<String, Value> converter) throws ExecutionException {
		Path path = resolvePath(image.getStringConstant(popI()));
		
		try {
			List<String> lines = Files.readAllLines(path);
			lines.removeIf(line -> line.trim().isEmpty());
			if(lines.size() < rows)
				throw new ExecutionException(ErrorCode.ioError, "Unable to read matrix: Too few rows", state.getRegI(CP));
			if(lines.size() > rows)
				throw new ExecutionException(ErrorCode.ioError, "Unable to read matrix: Too many rows", state.getRegI(CP));
			
			Value[][] values = new Value[rows][cols];
			
			for(int r = 0; r < rows; r++) {
				String[] row = lines.get(r).split("\\s*,\\s*");
				if(row.length < cols)
					throw new ExecutionException(ErrorCode.ioError, "Unable to read matrix: Too few columns", state.getRegI(CP));
				if(row.length > cols)
					throw new ExecutionException(ErrorCode.ioError, "Unable to read matrix: Too many columns", state.getRegI(CP));
				
				for(int c = 0; c < cols; c++) {
					try {
						values[r][c] = converter.apply(row[c].trim());
					} catch(NumberFormatException e) {
						throw new ExecutionException(ErrorCode.ioError, "Unable to read matrix: " + e.getLocalizedMessage(), state.getRegI(CP));
					}
				}
			}
			
			pushMatrix(values, rows, cols);
			
		} catch(IOException e) {
			throw new ExecutionException(ErrorCode.ioError, "Unable to read matrix: " + e.getLocalizedMessage(), state.getRegI(CP));
		}
	}
	
	private void performIntMatrixWrite(int rows, int cols) throws ExecutionException {
		performMatrixWrite(rows, cols, val -> String.valueOf(val.asInt()));
	}
	
	private void performFloatMatrixWrite(int rows, int cols) throws ExecutionException {
		performMatrixWrite(rows, cols, val -> String.valueOf(val.asFloat()).replace(',', '.'));
	}
	
	private void performMatrixWrite(int rows, int cols, Function<Value, String> converter) throws ExecutionException {
		try {
			Value[][] values = popMatrix(rows, cols);
			Path path = resolvePath(image.getStringConstant(popI()));
			if(path == null) return;
			
			Stream<String> s = Arrays.stream(values).map(row -> Arrays.stream(row).map(converter).collect(Collectors.joining(",")));
			
			Files.write(path, (Iterable<String>) s::iterator);
			
		} catch(IOException e) {
			throw new ExecutionException(ErrorCode.ioError, "Unable to write matrix: " + e.getLocalizedMessage(), state.getRegI(CP));
		}
	}
	
	public void callReadImage() throws ExecutionException {
		throw new ExecutionException(ErrorCode.internalError, "readImage is not implemented yet", state.getRegI(CP));
	}
	
	public void callWriteImage() throws ExecutionException {
		throw new ExecutionException(ErrorCode.internalError, "writeImage is not implemented yet", state.getRegI(CP));
	}
	
	public void callReadIntMatrix64() throws ExecutionException {
		performIntMatrixRead(64, 64);
	}
	
	public void callReadIntMatrix16() throws ExecutionException {
		performIntMatrixRead(16, 16);
	}
	
	public void callReadIntMatrix9() throws ExecutionException {
		performIntMatrixRead(9, 9);
	}
	
	public void callWriteIntMatrix64() throws ExecutionException {
		printMatrix(64, 64, false);
		performIntMatrixWrite(64, 64);
	}
	
	public void callWriteIntMatrix16() throws ExecutionException {
		printMatrix(16, 16, false);
		performIntMatrixWrite(16, 16);
	}
	
	public void callWriteIntMatrix9() throws ExecutionException {
		printMatrix(9, 9, false);
		performIntMatrixWrite(9, 9);
	}
	
	public void callReadFloatMatrix64() throws ExecutionException {
		performFloatMatrixRead(64, 64);
	}
	
	public void callReadFloatMatrix16() throws ExecutionException {
		performFloatMatrixRead(16, 16);
	}
	
	public void callReadFloatMatrix9() throws ExecutionException {
		performFloatMatrixRead(9, 9);
	}
	
	public void callWriteFloatMatrix64() throws ExecutionException {
		printMatrix(64, 64, true);
		performFloatMatrixWrite(64, 64);
	}
	
	public void callWriteFloatMatrix16() throws ExecutionException {
		printMatrix(16, 16, true);
		performFloatMatrixWrite(16, 16);
	}
	
	public void callWriteFloatMatrix9() throws ExecutionException {
		printMatrix(9, 9, true);
		performFloatMatrixWrite(9, 9);
	}
	
	public void callPowInt() throws ExecutionException {
		int e = popI();
		int b = popI();
		push((int) Math.pow(b, e));
	}
	
	public void callPowFloat() throws ExecutionException {
		float e = popF();
		float b = popF();
		push((float) Math.pow(b, e));
	}
	
	public void callSqrtInt() throws ExecutionException {
		int n = popI();
		push((int) Math.sqrt(n));
	}
	
	public void callSqrtFloat() throws ExecutionException {
		float n = popF();
		push((float) Math.sqrt(n));
	}
	
	public void callPrintInt() throws ExecutionException {
		int n = popI();
		output.print(n);
	}
	
	public void callPrintFloat() throws ExecutionException {
		float n = popF();
		output.print(n);
	}
	
	public void callPrintBool() throws ExecutionException {
		boolean b = popB();
		output.print(b);
	}
	
	public void callPrintString() throws ExecutionException {
		String s = image.getStringConstant(popI());
		output.print(s);
	}
	
	public void callPrintLine() {
		output.println();
	}
	
	public void callReadInt() throws ExecutionException {
		push(scanner.nextInt());
	}
	
	public void callReadFloat() throws ExecutionException {
		push(scanner.nextFloat());
	}
	
	public void callReadBool() throws ExecutionException {
		push(scanner.nextBoolean());
	}
	
	public void callInt2float() throws ExecutionException {
		int n = popI();
		push((float) n);
	}
	
	public void callFloat2int() throws ExecutionException {
		float n = popF();
		push((int) n);
	}
	
	public void callMatMulI() throws ExecutionException {
		matMul((acc, l, r) -> new Value(acc.asInt() + l.asInt() * r.asInt()));
	}
	
	public void callMatMulF() throws ExecutionException {
		matMul((acc, l, r) -> new Value(acc.asFloat() + l.asFloat() * r.asFloat()));
	}
	
	public void callMatTranspose() throws ExecutionException {
		// stack: mat, rows, cols
		int cols = popI();
		int rows = popI();
		
		Value[][] mat = popMatrix(rows, cols);
		Value[][] res = new Value[cols][rows];
		
		for(int r = 0; r < rows; r++) {
			for(int c = 0; c < cols; c++) {
				res[c][r] = mat[r][c];
			}
		}
		
		pushMatrix(res, cols, rows);
	}
	
	private void matMul(AggregateFunc func) throws ExecutionException {
		// stack: lmat, rmat, lrows, dim, rcols
		int cols = popI();
		int dim = popI();
		int rows = popI();
		
		Value[][] rmat = popMatrix(dim, cols);
		Value[][] lmat = popMatrix(rows, dim);
		Value[][] mat = new Value[rows][cols];
		
		for(int r = 0; r < rows; r++) {
			for(int c = 0; c < cols; c++) {
				Value sum = Value.zero;
				for(int i = 0; i < dim; i++) {
					sum = func.combine(sum, lmat[r][i], rmat[i][c]);
				}
				mat[r][c] = sum;
			}
		}
		
		pushMatrix(mat, rows, cols);
	}
	
	private interface AggregateFunc {
		Value combine(Value acc, Value l, Value r);
	}
	
	private Value[][] popMatrix(int rows, int cols) throws ExecutionException {
		int base = state.decStack(rows * cols);
		Value[][] mat = new Value[rows][cols];
		
		for(int r = 0; r < rows; r++)
			for(int c = 0; c < cols; c++)
				mat[r][c] = state.getMem(base + r * cols + c);
		
		return mat;
	}
	
	private void pushMatrix(Value[][] mat, int rows, int cols) throws ExecutionException {
		int base = state.incStack(rows * cols);
		
		for(int r = 0; r < rows; r++)
			for(int c = 0; c < cols; c++)
				state.setMem(base + r * cols + c, mat[r][c]);
	}
	
	private void printMatrix(int rows, int cols, boolean isFloat) throws ExecutionException {
		int mbase = state.getRegI(ST) - rows * cols;
		for(int r = 0; r < rows; r++) {
			for(int c = 0; c < cols; c++) {
				int o = mbase + r * cols + c;
				if(c != 0) output.print(", ");
				if(isFloat)
					output.print(state.getMemF(o));
				else
					output.print(state.getMemI(o));
			}
			output.println();
		}
	}
	
	// Helpers
	
	private void push(Value val) throws ExecutionException {
		state.pushStack(val);
	}
	
	private void push(int val) throws ExecutionException {
		state.pushStack(new Value(val));
	}
	
	private void push(float val) throws ExecutionException {
		state.pushStack(new Value(val));
	}
	
	private void push(boolean val) throws ExecutionException {
		state.pushStack(new Value(val));
	}
	
	private Value pop() throws ExecutionException {
		return state.popStack();
	}
	
	private int popI() throws ExecutionException {
		return state.popStack().asInt();
	}
	
	private float popF() throws ExecutionException {
		return state.popStack().asFloat();
	}
	
	private boolean popB() throws ExecutionException {
		return state.popStack().asBool();
	}
}
