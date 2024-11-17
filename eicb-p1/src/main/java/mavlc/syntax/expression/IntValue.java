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
import mavlc.type.IntType;
import mavlc.type.Type;

import java.util.Objects;

/**
 * AST node representation of an integer literal.
 */
public class IntValue extends Expression {
	
	public final int value;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param value The numeric value of this integer value.
	 */
	public IntValue(SourceLocation sourceLocation, int value) {
		super(sourceLocation);
		type = IntType.instance;
		this.value = value;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitIntValue(this, obj);
	}
	
	@Override
	public void setType(Type type) {
		if(!IntType.instance.equals(type)) {
			throw new UnsupportedOperationException("Cannot set another type than IntType for IntValue!");
		}
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		IntValue intValue = (IntValue) o;
		return value == intValue.value;
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), value);
	}
}
