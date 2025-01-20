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
package mtam;

import mavlc.errors.InternalCompilerError;
import mavlc.syntax.SourceLocation;
import mtam.debug.DebugSymbolContainer;
import mtam.debug.DebugSymbol;
import mtam.errors.ErrorCode;
import mtam.errors.ExecutionException;
import mtam.interpreter.Value;

import java.io.*;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static mavlc.util.TextUtil.padRight;
import static mtam.Register.PB;

public class Image {
	public Path imagePath;
	
	public String[] stringConstants;
	public Instruction[] instructions;
	
	public Image() { }
	
	public Image(Path imagePath) {
		this.imagePath = imagePath;
	}
	
	// getters
	
	public Instruction getInstruction(int address) throws ExecutionException {
		if(address < 0 || address >= instructions.length)
			throw new ExecutionException(ErrorCode.invalidAddress, "Unable to read instruction at address " + address, address);
		return instructions[address];
	}
	
	public String getStringConstant(int id) throws ExecutionException {
		if(id < 0 || id >= instructions.length)
			throw new ExecutionException(ErrorCode.internalError, "Unable to resolve string constant " + id, -1);
		return stringConstants[id];
	}
	
	public String getDisassemblyText() throws ExecutionException {
		return getDisassemblyText(false);
	}
	
	public String getDisassemblyText(boolean explicitPrimitiveCalls) throws ExecutionException {
		final int colWidthO = 7;
		final int colWidthN = 7;
		final int colWidthD = 9;
		
		StringBuilder sb = new StringBuilder();
		
		for(DisassemblyLine line : getDisassembly()) {
			switch(line.type) {
				case comment:
					sb.append("; ").append(line.text);
					break;
				case label:
					sb.append(line.text).append(':');
					break;
				case instruction:
					Instruction inst = line.inst;
					assert inst != null;
					
					sb.append(String.format("%04x: ", line.address));
					if(!explicitPrimitiveCalls && inst.op == Opcode.CALL && inst.r == PB) {
						String name = Primitive.fromDisplacement(inst.d).name();
						sb.append(name);
						sb.append(padRight("", colWidthO + colWidthN + colWidthD - name.length()));
					} else {
						sb.append(padRight(inst.op.name(), colWidthO));
						sb.append(padRight(inst.op.hasN ? "(" + inst.n + ")" : "", colWidthN));
						sb.append(padRight(inst.op.hasD ? inst.d + (inst.op.hasR ? "[" + inst.r.name() + "]" : "") : "", colWidthD));
					}
					
					for(DebugSymbol<?> sym : inst.debugInfo) {
						if(sym instanceof DebugSymbol.Name) {
							DebugSymbol.Name name = (DebugSymbol.Name) sym;
							sb.append(" ; ").append(name.value);
							break;
						}
					}
					break;
			}
			sb.append('\n');
		}
		
		return sb.toString();
	}
	
	public DisassemblyLine[] getDisassembly() {
		List<DisassemblyLine> text = new ArrayList<>();
		for(int addr = 0; addr < instructions.length; addr++) {
			Instruction inst = instructions[addr];
			for(DebugSymbol<?> entry : inst.debugInfo) {
				switch(entry.kind) {
					case comment:
						if(!((DebugSymbol.Comment) entry).showInDisasm) continue;
						String[] parts = ((String) entry.value).split("\n");
						for(String part : parts)
							text.add(new DisassemblyLine(addr, part, DisassemblyLine.Type.comment));
						break;
					case label:
						text.add(new DisassemblyLine(addr));
						text.add(new DisassemblyLine(addr, (String) entry.value, DisassemblyLine.Type.label));
						break;
				}
			}
			text.add(new DisassemblyLine(addr, inst));
		}
		return text.toArray(new DisassemblyLine[0]);
	}
	
	// file i/o
	
	public void save(OutputStream out) throws IOException {
		try(DataOutputStream writer = new DataOutputStream(out)) {
			saveInstructions(writer);
			saveStringConstants(writer);
		}
	}
	
	public void save(Path outPath) throws IOException {
		try(OutputStream out = new FileOutputStream(outPath.toFile())) {
			save(out);
		}
	}
	
	public void load() throws IOException, ExecutionException {
		try(DataInputStream reader = new DataInputStream(new FileInputStream(imagePath.toFile()))) {
			loadInstructions(reader);
			loadStringConstants(reader);
		}
	}
	
	public void saveSymbols(OutputStream out) throws IOException {
		int count = 0;
		for(Instruction inst : instructions) {
			if(!inst.debugInfo.isEmpty()) count++;
		}
		
		try(DataOutputStream writer = new DataOutputStream(out)) {
			writer.writeInt(count);
			saveSymbols(writer);
		}
	}
	
	public void saveSymbols(Path outPath) throws IOException {
		try(OutputStream out = new FileOutputStream(outPath.toFile())) {
			saveSymbols(out);
		}
	}
	
	public void loadSymbols(InputStream in) throws IOException {
		clearSymbols();
		try(DataInputStream reader = new DataInputStream(in)) {
			int count = reader.readInt();
			loadSymbols(reader, count);
		}
	}
	
	public void loadSymbols(Path inPath) throws IOException {
		try(InputStream in = new FileInputStream(inPath.toFile())) {
			loadSymbols(in);
		}
	}
	
	// sections
	
	private void saveInstructions(DataOutputStream writer) throws IOException {
		writer.writeInt(instructions.length);
		for(Instruction inst : instructions) {
			writeInstruction(writer, inst);
		}
	}
	
	private void loadInstructions(DataInputStream reader) throws IOException, ExecutionException {
		int count = reader.readInt();
		instructions = new Instruction[count];
		for(int i = 0; i < count; i++) {
			instructions[i] = readInstruction(reader);
		}
	}
	
	private void saveStringConstants(DataOutputStream writer) throws IOException {
		writer.writeInt(stringConstants.length);
		for(String constant : stringConstants) {
			writer.writeUTF(constant);
		}
	}
	
	private void loadStringConstants(DataInputStream reader) throws IOException {
		int count = reader.readInt();
		stringConstants = new String[count];
		for(int i = 0; i < count; i++) {
			stringConstants[i] = reader.readUTF();
		}
	}
	
	private void saveSymbols(DataOutputStream writer) throws IOException {
		for(int i = 0; i < instructions.length; i++) {
			Instruction inst = instructions[i];
			DebugSymbolContainer container = inst.debugInfo;
			if(container.isEmpty()) continue;
			
			writer.writeInt(i);
			writer.writeInt(container.size());
			for(DebugSymbol<?> symbol : container) {
				writeDebugSymbol(writer, symbol);
			}
		}
	}
	
	private void loadSymbols(DataInputStream reader, int count) throws IOException {
		for(int i = 0; i < count; i++) {
			int instOffset = reader.readInt();
			int entryCount = reader.readInt();
			for(int j = 0; j < entryCount; j++) {
				instructions[instOffset].debugInfo.add(readDebugSymbol(reader));
			}
		}
	}
	
	private void clearSymbols() {
		for(Instruction inst : instructions) {
			inst.debugInfo.clear();
		}
	}
	
	// serialization logic
	
	private void writeDebugSymbol(DataOutputStream writer, DebugSymbol<?> symbol) throws IOException {
		writer.writeInt(symbol.kind.id);
		switch(symbol.kind) {
			case name:
			case label:
				writer.writeUTF((String) symbol.value);
				break;
			case comment:
				writer.writeUTF((String) symbol.value);
				writer.writeBoolean(((DebugSymbol.Comment) symbol).showInDisasm);
				break;
			case location:
				SourceLocation loc = (SourceLocation) symbol.value;
				writer.writeInt(loc.line);
				writer.writeInt(loc.column);
				break;
			case type:
				writer.writeInt(((Value.Type) symbol.value).id);
				break;
			default:
				throw new InternalCompilerError("Invalid debug info entry: " + symbol.kind);
		}
	}
	
	private DebugSymbol<?> readDebugSymbol(DataInputStream reader) throws IOException {
		DebugSymbol.Kind kind = DebugSymbol.Kind.fromId(reader.readInt());
		switch(kind) {
			case comment:
				return new DebugSymbol.Comment(reader.readUTF(), reader.readBoolean());
			case location:
				return new DebugSymbol.Location(new SourceLocation(reader.readInt(), reader.readInt()));
			case type:
				return new DebugSymbol.Type(Value.Type.fromId(reader.readInt()));
			case name:
				return new DebugSymbol.Name(reader.readUTF());
			case label:
				return new DebugSymbol.Label(reader.readUTF());
			default:
				throw new IOException("Unsupported debug symbol: " + kind);
		}
	}
	
	private void writeInstruction(DataOutputStream writer, Instruction inst) throws IOException {
		writer.writeInt(inst.op.id);
		writer.writeInt(inst.r != null ? inst.r.id : 0);
		writer.writeInt(inst.n);
		writer.writeInt(inst.d);
	}
	
	private Instruction readInstruction(DataInputStream reader) throws IOException, ExecutionException {
		Opcode opcode = Opcode.fromId(reader.readInt());
		Register reg = Register.fromId(reader.readInt());
		int n = reader.readInt();
		int d = reader.readInt();
		return new Instruction(opcode, reg, n, d);
	}
}
