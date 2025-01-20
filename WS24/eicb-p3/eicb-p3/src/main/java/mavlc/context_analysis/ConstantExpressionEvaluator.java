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


public class ConstantExpressionEvaluator extends AstNodeBaseVisitor<Integer, Void> {
	@Override
	protected Integer defaultOperation(AstNode node, Void obj) {
		if(node instanceof Expression) {
			throw new NonConstantExpressionError((Expression) node);
		} else {
			throw new RuntimeException("Internal compiler error: should not try to constant-evaluate non-expressions");
		}
	}
	
	@Override
	public Integer visitIntValue(IntValue intValue, Void __) {
		return intValue.value;
	}
	
	
	// The Math.xxxExact methods are used to catch integer overflows.
	// The students are not expected to use them, there are no test cases testing for overflows.
	
	@Override
	public Integer visitUnaryMinus(UnaryMinus unaryMinus, Void __) {
		return Math.negateExact(unaryMinus.operand.accept(this));
	}
	
	@Override
	public Integer visitAddition(Addition addition, Void __) {
		return Math.addExact(addition.leftOperand.accept(this), addition.rightOperand.accept(this));
	}
	
	@Override
	public Integer visitSubtraction(Subtraction subtraction, Void __) {
		return Math.subtractExact(subtraction.leftOperand.accept(this), subtraction.rightOperand.accept(this));
	}
	
	@Override
	public Integer visitMultiplication(Multiplication multiplication, Void __) {
		return Math.multiplyExact(multiplication.leftOperand.accept(this), multiplication.rightOperand.accept(this));
	}
	
	@Override
	public Integer visitDivision(Division division, Void __) {
		return division.leftOperand.accept(this) / division.rightOperand.accept(this);
	}
	
	@Override
	public Integer visitExponentiation(Exponentiation exponentiation, Void __) {
		int base = exponentiation.leftOperand.accept(this);
		int exponent = exponentiation.rightOperand.accept(this);
		if(exponent < 0) throw new ArithmeticException();
		int pow = 1;
		for(int i = 0; i < exponent; ++i) {
			pow = Math.multiplyExact(pow, base);
		}
		return pow;
	}
}
