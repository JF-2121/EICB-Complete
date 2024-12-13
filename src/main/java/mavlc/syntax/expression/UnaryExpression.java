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

import java.util.Objects;

/**
 * Super type of all unary expressions, i.e. expressions of one operand.
 */
public abstract class UnaryExpression extends Expression {
	
	public final Expression operand;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param operand The operand.
	 */
	public UnaryExpression(SourceLocation sourceLocation, Expression operand) {
		super(sourceLocation);
		this.operand = operand;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitUnaryExpression(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		UnaryExpression that = (UnaryExpression) o;
		return Objects.equals(operand, that.operand);
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), operand);
	}
}
