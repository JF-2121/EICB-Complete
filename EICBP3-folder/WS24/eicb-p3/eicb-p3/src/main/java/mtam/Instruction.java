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

import mavlc.syntax.SourceLocation;
import mtam.debug.DebugSymbolContainer;
import mtam.interpreter.Value;

@SuppressWarnings({"unused", "UnusedReturnValue"})
public class Instruction {
	
	public final Opcode op;
	public final Register r;
	public final int n;
	public int d;
	
	public final DebugSymbolContainer debugInfo = new DebugSymbolContainer();
	
	public Instruction() {
		this(null, null, 0, 0);
	}
	
	public Instruction(Opcode op, Register r, int n, int d) {
		this.op = op;
		this.r = r;
		this.n = n;
		this.d = d;
	}
	
	public Instruction addComment(String comment) {
		debugInfo.addComment(comment);
		return this;
	}
	
	public Instruction addComment(String comment, boolean showInDisasm) {
		debugInfo.addComment(comment, showInDisasm);
		return this;
	}
	
	public Instruction addName(String name) {
		debugInfo.addName(name);
		return this;
	}
	
	public Instruction addLabel(String label) {
		debugInfo.addLabel(label);
		return this;
	}
	
	public Instruction addType(Value.Type type) {
		debugInfo.addType(type);
		return this;
	}
	
	public Instruction addLocation(SourceLocation location) {
		debugInfo.addLocation(location);
		return this;
	}
	
	public Instruction addBreakPoint() {
		debugInfo.addBreakPoint();
		return this;
	}
	
	public Instruction removeBreakPoint() {
		debugInfo.removeBreakPoint();
		return this;
	}
}
