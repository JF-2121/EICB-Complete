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
import mavlc.type.FloatType;
import mavlc.type.Type;

import java.util.Objects;

/**
 * AST node representation of a floating-point literal.
 */
public class FloatValue extends Expression {
	
	public final float value;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param value The floating-point value.
	 */
	public FloatValue(SourceLocation sourceLocation, float value) {
		super(sourceLocation);
		type = FloatType.instance;
		this.value = value;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitFloatValue(this, obj);
	}
	
	@Override
	public void setType(Type type) {
		if(!FloatType.instance.equals(type)) {
			throw new UnsupportedOperationException("Cannot set another type than FloatType for FloatValue!");
		}
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		FloatValue that = (FloatValue) o;
		return Float.compare(that.value, value) == 0;
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), value);
	}
}
