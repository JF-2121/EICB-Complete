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
 * AST node representing the comparison of two values, yielding a boolean value.
 */
public class Compare extends BinaryExpression {
	
	public final Comparison comparator;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param leftOperand The left operand of the comparison.
	 * @param rightOperand The right operand of the comparison.
	 * @param comparator The comparator of the comparison.
	 */
	public Compare(SourceLocation sourceLocation, Expression leftOperand, Expression rightOperand, Comparison comparator) {
		super(sourceLocation, leftOperand, rightOperand);
		this.comparator = comparator;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitCompare(this, obj);
	}
	
	/** Possible comparators for comparisons. */
	public enum Comparison {
		LESS("<"),
		GREATER(">"),
		LESS_EQUAL("<="),
		GREATER_EQUAL(">="),
		NOT_EQUAL("!="),
		EQUAL("==");
		
		public final String operator;
		
		Comparison(String operator) {
			this.operator = operator;
		}
	}
}
