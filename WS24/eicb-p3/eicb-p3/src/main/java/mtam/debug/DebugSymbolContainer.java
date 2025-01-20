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
package mtam.debug;

import mavlc.syntax.SourceLocation;
import mtam.interpreter.Value;

import java.util.ArrayList;

@SuppressWarnings("UnusedReturnValue")
public class DebugSymbolContainer extends ArrayList<DebugSymbol<?>> {
	
	public DebugSymbolContainer addComment(String comment) {
		add(new DebugSymbol.Comment(comment));
		return this;
	}
	
	public DebugSymbolContainer addComment(String comment, boolean showInDisasm) {
		add(new DebugSymbol.Comment(comment, showInDisasm));
		return this;
	}
	
	public DebugSymbolContainer addName(String name) {
		add(new DebugSymbol.Name(name));
		return this;
	}
	
	public DebugSymbolContainer addLabel(String label) {
		add(new DebugSymbol.Label(label));
		return this;
	}
	
	public DebugSymbolContainer addType(Value.Type type) {
		add(new DebugSymbol.Type(type));
		return this;
	}
	
	public DebugSymbolContainer addLocation(SourceLocation location) {
		add(new DebugSymbol.Location(location));
		return this;
	}
	
	public DebugSymbolContainer addBreakPoint() {
		if(!hasBreakPoint())
			add(new DebugSymbol.BreakPoint());
		return this;
	}
	
	public DebugSymbolContainer removeBreakPoint() {
		removeIf(sym -> sym.kind == DebugSymbol.Kind.breakPoint);
		return this;
	}
	
	public boolean hasBreakPoint() {
		for(DebugSymbol<?> symbol : this) {
			if(symbol.kind == DebugSymbol.Kind.breakPoint) {
				return true;
			}
		}
		return false;
	}
	
	public Value.Type getType() {
		Value.Type type = Value.Type.unknown;
		for(DebugSymbol<?> entry : this) {
			if(entry.kind == DebugSymbol.Kind.type) {
				type = (Value.Type) entry.value;
			}
		}
		return type;
	}
	
	public SourceLocation getLocation() {
		SourceLocation location = SourceLocation.unknown;
		for(DebugSymbol<?> entry : this) {
			if(entry.kind == DebugSymbol.Kind.location) {
				location = (SourceLocation) entry.value;
			}
		}
		return location;
	}
}
