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

public abstract class DebugSymbol<T> {
	public final Kind kind;
	public final T value;
	
	protected DebugSymbol(Kind kind, T value) {
		this.kind = kind;
		this.value = value;
	}
	
	public static final class Comment extends DebugSymbol<String> {
		public final boolean showInDisasm;
		public Comment(String value) {
			super(Kind.comment, value);
			this.showInDisasm = true;
		}
		public Comment(String value, boolean showInDisasm) {
			super(Kind.comment, value);
			this.showInDisasm = showInDisasm;
		}
	}
	
	public static final class Location extends DebugSymbol<SourceLocation> {
		public Location(SourceLocation value) {
			super(Kind.location, value);
		}
	}
	
	public static final class Type extends DebugSymbol<Value.Type> {
		public Type(Value.Type value) {
			super(Kind.type, value);
		}
	}
	
	public static final class Name extends DebugSymbol<String> {
		public Name(String value) {
			super(Kind.name, value);
		}
	}
	
	public static final class Label extends DebugSymbol<String> {
		public Label(String value) {
			super(Kind.label, value);
		}
	}
	
	public static final class BreakPoint extends DebugSymbol<Boolean> {
		public BreakPoint() {
			super(Kind.breakPoint, true);
		}
	}
	
	public enum Kind {
		comment(1), location(2), type(3), name(4), label(5), breakPoint(6);
		
		public final int id;
		
		Kind(int id) {this.id = id;}
		
		private static final Kind[] values = values();
		
		public static Kind fromId(int id) {
			for(Kind kind : values) {
				if(kind.id == id) return kind;
			}
			throw new IllegalArgumentException("Invalid DebugSymbol.Kind: " + id);
		}
	}
}
