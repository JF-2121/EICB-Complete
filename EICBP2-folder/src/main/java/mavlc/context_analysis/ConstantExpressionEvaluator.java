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
 * EiCB group number: 149
 * Joshua Liam Friedel (279635)
 * Benedikt Schwarz (2373528)
 * Lasse Ramon Reith (2674146)
 */

/**
 * This class evaluates constant expressions in the AST.
 * It extends the base visitor to traverse and compute values for constant expressions.
 */
public class ConstantExpressionEvaluator extends AstNodeBaseVisitor<Integer, Void> {

	/**
	 * Handles cases where a node is not recognized as an expression.
	 * Throws an error if a non-constant expression is encountered.
	 *
	 * @param node    The AST node to process.
	 * @param context The evaluation context (unused).
	 * @return Never returns a value, always throws an exception.
	 */
	@Override
	protected Integer defaultOperation(AstNode node, Void context) {
		if (node instanceof Expression) {
			throw new NonConstantExpressionError((Expression) node);
		}
		throw new RuntimeException("Internal compiler error: Unexpected attempt to evaluate non-expressions");
	}

	/**
	 * Evaluates an integer value.
	 *
	 * @param intValue The integer value node.
	 * @param context  The evaluation context (unused).
	 * @return The integer value.
	 */
	@Override
	public Integer visitIntValue(IntValue intValue, Void context) {
		return intValue.value;
	}

	/**
	 * Evaluates an addition operation.
	 *
	 * @param addition The addition node.
	 * @param context  The evaluation context (unused).
	 * @return The sum of the left and right operands.
	 */
	@Override
	public Integer visitAddition(Addition addition, Void context) {
		int left = addition.leftOperand.accept(this);
		int right = addition.rightOperand.accept(this);
		return left + right;
	}

	/**
	 * Evaluates a subtraction operation.
	 *
	 * @param subtraction The subtraction node.
	 * @param context     The evaluation context (unused).
	 * @return The difference of the left and right operands.
	 */
	@Override
	public Integer visitSubtraction(Subtraction subtraction, Void context) {
		int left = subtraction.leftOperand.accept(this);
		int right = subtraction.rightOperand.accept(this);
		return left - right;
	}

	/**
	 * Evaluates a multiplication operation.
	 *
	 * @param multiplication The multiplication node.
	 * @param context         The evaluation context (unused).
	 * @return The product of the left and right operands.
	 */
	@Override
	public Integer visitMultiplication(Multiplication multiplication, Void context) {
		int left = multiplication.leftOperand.accept(this);
		int right = multiplication.rightOperand.accept(this);
		return left * right;
	}

	/**
	 * Evaluates a division operation.
	 *
	 * @param division The division node.
	 * @param context  The evaluation context (unused).
	 * @return The quotient of the left and right operands.
	 */
	@Override
	public Integer visitDivision(Division division, Void context) {
		int left = division.leftOperand.accept(this);
		int right = division.rightOperand.accept(this);
		return left / right;
	}

	/**
	 * Evaluates an exponentiation operation.
	 *
	 * @param exponentiation The exponentiation node.
	 * @param context        The evaluation context (unused).
	 * @return The result of raising the left operand to the power of the right operand.
	 */
	@Override
	public Integer visitExponentiation(Exponentiation exponentiation, Void context) {
		int base = exponentiation.leftOperand.accept(this);
		int exponent = exponentiation.rightOperand.accept(this);
		return (int) Math.pow(base, exponent);
	}

	/**
	 * Evaluates a unary minus operation.
	 *
	 * @param unaryMinus The unary minus node.
	 * @param context    The evaluation context (unused).
	 * @return The negation of the operand.
	 */
	@Override
	public Integer visitUnaryMinus(UnaryMinus unaryMinus, Void context) {
		int operand = unaryMinus.operand.accept(this);
		return -operand;
	}
}
