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
package mavlc.context_analysis;

import mavlc.errors.NonConstantExpressionError;
import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeBaseVisitor;
import mavlc.syntax.expression.*;

/* TODO enter group information
 *
 * EiCB group number: ...
 * Names and matriculation numbers of all group members:
 * ...
 */

public class ConstantExpressionEvaluator extends AstNodeBaseVisitor<Integer, Void> {

	@Override
	protected Integer defaultOperation(AstNode node, Void context) {
		if (node instanceof Expression) {
			throw new NonConstantExpressionError((Expression) node);
		}
		throw new RuntimeException("Internal compiler error: Unexpected attempt to evaluate non-expressions");
	}

	@Override
	public Integer visitIntValue(IntValue intValue, Void context) {
		return intValue.value;
	}

	@Override
	public Integer visitAddition(Addition addition, Void context) {
		int left = addition.leftOperand.accept(this);
		int right = addition.rightOperand.accept(this);
		return left + right;
	}

	@Override
	public Integer visitSubtraction(Subtraction subtraction, Void context) {
		int left = subtraction.leftOperand.accept(this);
		int right = subtraction.rightOperand.accept(this);
		return left - right;
	}

	@Override
	public Integer visitMultiplication(Multiplication multiplication, Void context) {
		int left = multiplication.leftOperand.accept(this);
		int right = multiplication.rightOperand.accept(this);
		return left * right;
	}

	@Override
	public Integer visitDivision(Division division, Void context) {
		int left = division.leftOperand.accept(this);
		int right = division.rightOperand.accept(this);
		return left / right;
	}

	@Override
	public Integer visitExponentiation(Exponentiation exponentiation, Void context) {
		int base = exponentiation.leftOperand.accept(this);
		int exponent = exponentiation.rightOperand.accept(this);
		return (int) Math.pow(base, exponent);
	}

	@Override
	public Integer visitUnaryMinus(UnaryMinus unaryMinus, Void context) {
		int operand = unaryMinus.operand.accept(this);
		return -operand;
	}
}
