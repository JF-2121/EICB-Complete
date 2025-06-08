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
 * Super type of all binary expression, i.e. expressions of two operands.
 */
public class BinaryExpression extends Expression {
	
	public final Expression leftOperand;
	public final Expression rightOperand;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param leftOperand Left operand.
	 * @param rightOperand Right operand.
	 */
	public BinaryExpression(SourceLocation sourceLocation, Expression leftOperand, Expression rightOperand) {
		super(sourceLocation);
		this.leftOperand = leftOperand;
		this.rightOperand = rightOperand;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitBinaryExpression(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		BinaryExpression that = (BinaryExpression) o;
		return Objects.equals(leftOperand, that.leftOperand) &&
				Objects.equals(rightOperand, that.rightOperand);
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), leftOperand, rightOperand);
	}
}
