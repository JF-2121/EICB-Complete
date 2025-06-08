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
package mavlc.syntax.expression;

import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.SourceLocation;
import mavlc.type.BoolType;
import mavlc.type.Type;

import java.util.Objects;

/**
 * AST node representing a boolean literal, i.e. true or false.
 */
public class BoolValue extends Expression {
	
	public final boolean value;
	
	/**
	 * Constructs a boolean value from a truth value.
	 *
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param value Truth value.
	 */
	public BoolValue(SourceLocation sourceLocation, boolean value) {
		super(sourceLocation);
		type = BoolType.instance;
		this.value = value;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitBoolValue(this, obj);
	}
	
	@Override
	public void setType(Type type) {
		if(!BoolType.instance.equals(type)) {
			throw new UnsupportedOperationException("Cannot set another type than BoolType for BoolValue!");
		}
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		BoolValue boolValue = (BoolValue) o;
		return value == boolValue.value;
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), value);
	}
}
