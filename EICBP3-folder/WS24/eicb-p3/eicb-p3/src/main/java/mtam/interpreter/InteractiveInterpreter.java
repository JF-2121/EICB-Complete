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

import mavlc.Pipeline;
import mavlc.util.Ansi;
import mtam.*;
import mtam.debug.DebugSymbol;
import mtam.debug.DebugSymbolContainer;
import mtam.errors.ErrorCode;
import mtam.errors.ExecutionException;

import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static mavlc.util.TextUtil.padLeft;
import static mavlc.util.TextUtil.padRight;
import static mtam.Register.*;

@SuppressWarnings("SameParameterValue")
public class InteractiveInterpreter extends Interpreter {
	
	private static final Register[] registers = Register.values();
	
	private static final int colWidthO = 7;
	private static final int colWidthN = 7;
	private static final int colWidthD = 9;
	private static final int colWidthC = 10;
	private static final int colWidthS = 26;
	
	public InteractiveInterpreter() {
		this(System.in, System.out);
	}
	
	public InteractiveInterpreter(InputStream input, PrintStream output) {
		super(input, output);
	}
	
	public static void main(String[] args) {
        System.setOut(new PrintStream(System.out, true, StandardCharsets.UTF_8));
        new InteractiveInterpreter().run(args.length > 0 ? args[0] : null);
	}
	
	public void run(String imagePath) {
		if(System.getProperty("os.name").toLowerCase().contains("win") && System.getenv("MAVL_COLOR") == null) {
			Ansi.disable();
			boxes = false;
			output.println("=====================================================================================");
			output.println("             WARNING: Color output not supported under windows.");
			output.println(" To enable it anyway (e.g. in IntelliJ), define the environment variable MAVL_COLOR.");
			output.println("=====================================================================================");
		} else {
			Ansi.enable();
		}
		
		try {
			executeLoadCmd(imagePath);
			commandLoop();
		} catch(ExecutionException e) {
			state.raiseError(e);
		}
		printStatus();
		scanner.nextLine();
	}
	
	private void commandLoop() {
		while(state.executionState == ExecutionState.running) {
			try {
				String[] parts = scanner.nextLine().split("\\s+");
				String command = parts.length == 0 ? "step" : parts[0];
				switch(command.toLowerCase()) {
					case "":
					case "step":
						executeStepCmd();
						break;
					case "run":
					case "continue":
						executeRunCmd();
						break;
					case "runto":
						if(parts.length >= 2)
							executeRunToCmd(parseInt(parts[1]));
						else
							output.println(Ansi.red + "No address specified" + Ansi.reset);
						break;
					case "info":
						if(parts.length >= 2)
							executeInfoCmd(parseInt(parts[1]));
						else
							executeInfoCmd(getCurrentInstructionAddress());
						break;
					case "break":
						if(parts.length >= 2)
							executeBreakCmd(parseInt(parts[1]));
						else
							executeBreakCmd(getCurrentInstructionAddress());
						break;
					case "load":
						if(parts.length >= 2)
							executeLoadCmd(parts[1]);
						else
							executeLoadCmd(null);
						break;
					case "?":
					case "help":
						executeHelpCmd();
						break;
					case "quit":
					case "exit":
					case "halt":
					case "stop":
					case "end":
						state.executionState = ExecutionState.terminated;
						return;
					default:
						output.println("Unrecognized command: " + command);
						break;
				}
			} catch(ExecutionException e) {
				state.raiseError(e);
			}
		}
	}
	
	private static int parseInt(String str) {
		if(str.startsWith("0x") || str.startsWith("0X")) {
			return Integer.parseInt(str.substring(2), 16);
		}
		return Integer.parseInt(str);
	}
	
	private void executeStepCmd() throws ExecutionException {
		advance();
		printState();
	}
	
	private void executeRunCmd() throws ExecutionException {
		while(state.executionState == ExecutionState.running) {
			// The advance has to be done first, to advance execution past a breakpoint that has
			// been hit before.
			// This will skip breakpoints at address 0x0.
			advance();
			if(getCurrentInstruction().debugInfo.stream().anyMatch(sym -> sym.kind == DebugSymbol.Kind.breakPoint)) {
				printState();
				output.println("Breakpoint triggered");
				break;
			}
		}
	}
	
	private void executeRunToCmd(int address) throws ExecutionException {
		while(state.executionState == ExecutionState.running && getCurrentInstructionAddress() != address) {
			// The advance has to be done first, to advance execution past a breakpoint that has
			// been hit before.
			// This will skip breakpoints at address 0x0.
			advance();
			if(getCurrentInstruction().debugInfo.stream().anyMatch(sym -> sym.kind == DebugSymbol.Kind.breakPoint)) {
				printState();
				output.println("Breakpoint triggered");
				return;
			}
		}
		printState();
	}
	
	private void executeInfoCmd(int address) throws ExecutionException {
		Instruction inst = image.getInstruction(address);
		output.print(Ansi.brightCyan + padLeft(Integer.toHexString(address), 4, '0') + ": " + Ansi.reset);
		printInstruction(inst, false);
		output.println();
		
		DebugSymbolContainer container = inst.debugInfo;
		if(container.isEmpty()) {
			output.println("No debug symbols are attached to this instruction");
		} else {
			output.println("Found " + container.size() + " debug symbols attached to this instruction:");
			for(DebugSymbol<?> symbol : container) {
				output.println("" + Ansi.yellow + symbol.kind + ": " + Ansi.reset + symbol.value);
			}
		}
	}
	
	private void executeBreakCmd(int address) throws ExecutionException {
		Instruction inst = image.getInstruction(address);
		if(isBreakPointSet(inst)) {
			unsetBreakPoint(inst);
			output.println("Removed breakpoint from address " + padLeft(Integer.toHexString(address), 4, '0'));
		} else {
			setBreakPoint(inst);
			output.println("Added breakpoint to address " + padLeft(Integer.toHexString(address), 4, '0'));
		}
		printState();
	}
	
	private void executeLoadCmd(String filename) throws ExecutionException {
		if(filename == null) {
			output.print("Please enter path to the program image.\n> ");
			filename = scanner.nextLine();
			if(filename.startsWith("\"") && filename.endsWith("\"")) filename = filename.substring(1, filename.length() - 1);
		}
		
		var path = Paths.get(filename);
		basePath = path.getParent();

		Image image;
		try {
			if(filename.endsWith(".mavl")) {
				Pipeline pipeline = new Pipeline();
				pipeline.parseProgram(path);
				pipeline.throwError();
				pipeline.analyzeProgram();
				pipeline.throwError();
				pipeline.compileProgram();
				pipeline.throwError();
				image = pipeline.getImage();
			} else {
				image = new Image(path);
				image.load();
				state.throwIfError();
				
				Path symPath = Paths.get(filename.replace(".tam", ".sym"));
				if(Files.exists(symPath)) {
					output.print("Debug symbols found, load them now? (y/n)\n> ");
					if(scanner.nextLine().startsWith("y")) {
						output.print("Loading debug symbols");
						image.loadSymbols(symPath);
					}
				}
			}
		} catch(Exception e) {
			state.raiseError(ErrorCode.ioError, "Failed to load program: " + e.getMessage(), -1);
			return;
		}
		
		loadImage(image);
		printState();
		output.println("Type " + Ansi.brightGreen + "help" + Ansi.reset + " for a list of available commands");
	}
	
	private void executeHelpCmd() {
		output.println("The following commands are available:");
		output.println("- "+ Ansi.green + "step         " + Ansi.reset + " executes a single instruction");
		output.println("- "+ Ansi.green + "run          " + Ansi.reset + " continues execution until a break point is hit (alias: " + Ansi.green + "continue" + Ansi.reset + ")");
		output.println("- "+ Ansi.green + "runto " + Ansi.brightYellow + "address" + Ansi.reset + " runs until the specified instruction is hit, ignoring break points");
		output.println("- "+ Ansi.green + "info         " + Ansi.reset + " prints detailed information about the current instruction");
		output.println("- "+ Ansi.green + "info " + Ansi.brightYellow + "address " + Ansi.reset + " prints detailed information about the specified instruction");
		output.println("- "+ Ansi.green + "break        " + Ansi.reset + " toggles break point at the current instruction");
		output.println("- "+ Ansi.green + "break " + Ansi.brightYellow + "address" + Ansi.reset + " toggles break point at the specified instruction");
		output.println("- "+ Ansi.green + "load         " + Ansi.reset + " opens 'load image' prompt");
		output.println("- "+ Ansi.green + "load " + Ansi.brightYellow + "path    " + Ansi.reset + " loads the specified image");
		output.println("- "+ Ansi.green + "help         " + Ansi.reset + " displays a short summary of the available commands (alias: " + Ansi.green + "?" + Ansi.reset + ")");
		output.println("- "+ Ansi.green + "exit         " + Ansi.reset + " quits the interactive interpreter (aliases: " + Ansi.green + "quit" + Ansi.reset + ", " + Ansi.green + "halt" + Ansi.reset + ", " + Ansi.green + "stop" + Ansi.reset + ", " + Ansi.green + "end" + Ansi.reset + ")");
		output.println("If no command was entered, " + Ansi.green + "step" + Ansi.reset + " is executed by default.");
	}
	
	private boolean isBreakPointSet(Instruction inst) {
		return inst.debugInfo.hasBreakPoint();
	}
	
	private void setBreakPoint(Instruction inst) {
		inst.debugInfo.addBreakPoint();
	}
	
	private void unsetBreakPoint(Instruction inst) {
		inst.debugInfo.removeBreakPoint();
	}
	
	private DisassemblyLine[] disassembly;
	private int[] imageTextMap;
	
	private final int lineCount = 25;
	private boolean boxes = true;
	
	@Override public void loadImage(Image image) {
		super.loadImage(image);
		disassembly = image.getDisassembly();
		imageTextMap = new int[image.instructions.length];
		for(int i = 0; i < disassembly.length; i++) {
			DisassemblyLine line = disassembly[i];
			if(line.type == DisassemblyLine.Type.instruction) {
				imageTextMap[line.address] = i;
			}
		}
	}
	
	private void printState() throws ExecutionException {
		String indent = "        ";
		// scroll down
		for(int i = 0; i < 10; i++) output.println();
		// headers
		output.print(indent);
		output.println(boxes
				? "\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550 Program Image \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2564\u2550\u2550\u2550\u2550\u2550\u2550\u2550 Registers \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2564\u2550\u2550\u2550\u2550\u2550 Stack Contents \u2550\u2550\u2550\u2550\u2550\u2557"
				: "#======== Program Image ========#======= Registers =======#===== Stack Contents =====#");
		for(int i = 0; i < lineCount - 2; i++) {
			output.print(indent);
			
			output.print(boxes ? '\u2551' : "|");
			
			// print disassembly
			printDisassemblyLine(i);
			
			output.print(boxes ? '\u2502' : "|");
			
			// registers
			printRegisterLine(i);
			
			output.print(boxes ? '\u2502' : "|");
			
			// stack
			printStackLine(i);
			
			output.print(boxes ? '\u2551' : "|");
			output.println();
		}
		// footers
		output.print(indent);
		output.println(boxes
				? "\u2559\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2534\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2534\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u255c"
				: "#===============================#=========================#==========================#");
	}
	
	private void printDisassemblyLine(int displayLine) throws ExecutionException {
		final int activeLine = 5;
		
		int cp = state.registers[CP.id].bits;
		int lineNumber = imageTextMap[cp] + displayLine - activeLine;
		boolean isActive = displayLine == activeLine;
		if(lineNumber >= 0 && lineNumber < disassembly.length) {
			DisassemblyLine line = disassembly[lineNumber];
			switch(line.type) {
				case comment:
					output.print(Ansi.cyan);
					output.print(padRight("; " + line.text, 6 + colWidthO + colWidthN + colWidthD + 2));
					output.print(Ansi.reset);
					return;
				case label:
					output.print(Ansi.brightMagenta);
					output.print(padRight(line.text + ":", 6 + colWidthO + colWidthN + colWidthD + 2));
					output.print(Ansi.reset);
					return;
				case instruction:
					Instruction inst = line.inst;
					boolean breakPoint = isBreakPointSet(inst);
					// markers
					output.print(breakPoint
							? isActive ? "" + Ansi.brightRed + Ansi.invert + "~>" : "" + Ansi.brightRed + Ansi.invert + "B" + Ansi.reset + " "
							: isActive ? "" + Ansi.brightRed + Ansi.invert + "~>" : "  ");
					// address
					output.print(isActive ? "" + Ansi.white + Ansi.invert : Ansi.brightCyan);
					output.printf("%04x: ", line.address);
					// instruction
					if(isActive) {
						printInstruction(inst, true);
					} else {
						output.print(Ansi.reset);
						printInstruction(inst, false);
					}
					output.print(Ansi.reset);
					return;
			}
		}
		output.print(padRight("", 6 + colWidthO + colWidthN + colWidthD + 2));
	}
	
	private void printRegisterLine(int displayLine) {
		if(displayLine < registers.length) {
			Register reg = registers[displayLine];
			output.print(" " + reg.name() + ": ");
			String hex = Integer.toHexString(state.registers[displayLine].bits);
			if(hex.equals("0")) hex = "";
			String padding = padRight("", 8 - hex.length(), '0');
			output.print(Ansi.brightBlack);
			output.print(padding);
			output.print(Ansi.reset);
			output.print(hex);
			output.print(' ');
			switch(reg) {
				case CT:
				case PB:
				case CP:
					output.print(padLeft((state.registers[reg.id].bits - state.registers[CB.id].bits) + "[CB]", colWidthC));
					break;
				case PT:
					output.print(padLeft((state.registers[reg.id].bits - state.registers[PB.id].bits) + "[PB]", colWidthC));
					break;
				case ST:
				case LB:
					output.print(padLeft((state.registers[reg.id].bits - state.registers[SB.id].bits) + "[SB]", colWidthC));
					break;
				default:
					output.print(padLeft("", colWidthC));
			}
			output.print(' ');
		} else {
			output.print(padRight("", 15 + colWidthC));
		}
	}
	
	private void printStackLine(int displayLine) {
		final int blankStackRows = 2;
		
		int st = state.registers[ST.id].bits;
		int overflow = Math.max(0, st - lineCount + 2 + blankStackRows);
		int stackAddress = displayLine + overflow;
		boolean isActive = stackAddress < st;
		if(overflow > 0 && displayLine == 0) {
			output.print(Ansi.brightRed);
			output.print(padRight(" ^^^^  ^^^^^^^^ " + (overflow + 1) + " more", colWidthS));
			output.print(Ansi.reset);
		} else if(stackAddress >= 0 && stackAddress < state.memory.length) {
			// address
			output.print(stackAddress < st ? Ansi.brightGreen : Ansi.brightBlack);
			output.printf(" %04x: ", stackAddress);
			// contents
			Value val = state.memory[stackAddress];
			if(val == null) val = Value.zero;
			String hex = Integer.toHexString(val.bits);
			if(hex.equals("0")) hex = "";
			String padding = padRight("", 8 - hex.length(), '0');
			if(!padding.isEmpty()) {
				output.print(isActive ? Ansi.white : Ansi.brightBlack);
				output.print(padding);
			}
			output.print(isActive ? Ansi.reset : Ansi.brightBlack);
			output.print(hex);
			output.print(' ');
			output.print(Ansi.reset);
			printValue(state.memory[stackAddress], colWidthS - 17, isActive);
			output.print(Ansi.reset);
			output.print(' ');
		} else {
			output.print(padRight("", colWidthS));
		}
	}
	
	private void printValue(Value value, int width, boolean isActive) {
		if(!isActive) {
			if(Ansi.isEnabled) {
				output.print(Ansi.brightBlack);
			} else {
				output.print(padLeft("", width, '/'));
				return;
			}
		}
		if(value == null) {
			output.print(padLeft("", width));
			return;
		}
		switch(value.type) {
			case intT:
				output.print(padLeft(String.valueOf(value.asInt()), width));
				break;
			case boolT:
				output.print(padLeft(String.valueOf(value.asBool()), width));
				break;
			case floatT:
				output.print(padLeft(value.asFloat() + "f", width));
				break;
			case cAddrT:
				output.print(padLeft((value.bits - state.registers[CB.id].bits) + "[CB]", width));
				break;
			case sAddrT:
				output.print(padLeft((value.bits - state.registers[SB.id].bits) + "[SB]", width));
				break;
			case stringT:
				output.print(padLeft("$" + value.bits, width));
				break;
			default:
				output.print(padLeft("", width));
				break;
		}
	}
	
	private void printInstruction(Instruction inst, boolean isActive) throws ExecutionException {
		if(inst.op == Opcode.CALL && inst.r == PB) {
			output.print(Ansi.brightYellow);
			if(isActive) output.print(Ansi.invert);
			String name = Primitive.fromDisplacement(inst.d).name();
			output.print(name);
			output.print(Ansi.resetColor);
			if(isActive) output.print(Ansi.invert);
			output.print(padRight("", colWidthO + colWidthN + colWidthD - name.length()));
			output.print(Ansi.resetColor);
		} else {
			boolean printN = inst.op.hasN;
			boolean printD = inst.op.hasD;
			boolean printR = inst.op.hasR;
			
			output.print(padRight(inst.op.name(), colWidthO));
			output.print(padRight(printN ? "(" + inst.n + ")" : "", colWidthN));
			output.print(padRight(printD ? inst.d + (printR ? "[" + inst.r.name() + "]" : "") : "", colWidthD));
		}
	}
}
