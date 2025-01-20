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

import mtam.Opcode;
import mtam.Primitive;
import mtam.Register;

import static mavlc.util.TextUtil.padRight;
import static mavlc.util.TextUtil.padLeft;

public class Profiler {
	
	public final int[] regReadCount = new int[Register.count];
	public final int[] regWriteCount = new int[Register.count];
	public final int[] opcodeExecCount = new int[Opcode.count];
	public final int[] primitiveExecCount = new int[Primitive.count];
	
	public int memReadCount;
	public int memWriteCount;
	public int memCopyCount;
	public int memZeroCount;
	
	public void exec(Opcode op) {
		opcodeExecCount[op.id]++;
	}
	
	public void exec(Primitive prim) {
		primitiveExecCount[prim.displacement]++;
	}
	
	public void regRead(Register reg) {
		regReadCount[reg.id]++;
	}
	
	public void regWrite(Register reg) {
		regWriteCount[reg.id]++;
	}
	
	public void memRead() {
		memReadCount++;
	}
	
	public void memWrite() {
		memWriteCount++;
	}
	
	public void memCopy() {
		memCopyCount++;
	}
	
	public void memZero() {
		memZeroCount++;
	}
	
	public String generateReport(boolean compact) {
		final int nameColWidth = 15;
		
		StringBuilder sb = new StringBuilder();
		sb.append("Opcode executions:\n");
		for(Opcode op : Opcode.values()) {
			if(compact && opcodeExecCount[op.id] == 0) continue;
			sb.append(padRight(op.name(), nameColWidth));
			appendNumber(sb, opcodeExecCount[op.id]);
			sb.append("\n");
		}
		sb.append("\n");
		sb.append("Primitive executions:\n");
		for(Primitive prim : Primitive.values()) {
			if(compact && primitiveExecCount[prim.displacement] == 0) continue;
			sb.append(padRight(prim.name(), nameColWidth));
			appendNumber(sb, primitiveExecCount[prim.displacement]);
			sb.append("\n");
		}
		sb.append("\n");
		sb.append("Register reads / writes:\n");
		for(Register reg : Register.values()) {
			if(compact && regReadCount[reg.id] == 0 && regWriteCount[reg.id] == 0) continue;
			sb.append(padRight(reg.name(), nameColWidth));
			appendNumber(sb, regReadCount[reg.id]);
			sb.append(" / ");
			appendNumber(sb, regWriteCount[reg.id]);
			sb.append("\n");
		}
		sb.append("\n");
		sb.append(padRight("Memory reads:", nameColWidth));
		appendNumber(sb, memReadCount);
		sb.append("\n");
		sb.append(padRight("Memory writes:", nameColWidth));
		appendNumber(sb, memWriteCount);
		sb.append("\n");
		sb.append(padRight("Memory copies:", nameColWidth));
		appendNumber(sb, memCopyCount);
		sb.append("\n");
		sb.append(padRight("Memory clears:", nameColWidth));
		appendNumber(sb, memZeroCount);
		sb.append("\n");
		return sb.toString();
	}
	
	private void appendNumber(StringBuilder sb, int num) {
		sb.append(num == 0 ? padLeft("-", 5) : num > 9999 ? ">9999" : padLeft(String.valueOf(num), 5));
	}
}
