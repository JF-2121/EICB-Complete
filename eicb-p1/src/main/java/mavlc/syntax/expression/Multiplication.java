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

/**
 * AST node representing a multiplication, including multiplications of
 * a matrix or vector with a scalar value and element-wise multiplications
 * of matrices or vectors.
 */
public class Multiplication extends BinaryExpression {
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param leftOperand The left operand of this multiplication.
	 * @param rightOperand The right operand of this multiplication.
	 */
	public Multiplication(SourceLocation sourceLocation, Expression leftOperand, Expression rightOperand) {
		super(sourceLocation, leftOperand, rightOperand);
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitMultiplication(this, obj);
	}
}
