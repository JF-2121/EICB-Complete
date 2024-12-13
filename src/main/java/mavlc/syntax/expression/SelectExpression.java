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
 * AST node representing a ternary ?-expression.
 */
public class SelectExpression extends Expression {
	public final Expression condition;
	public final Expression trueCase;
	public final Expression falseCase;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param condition Condition that determines which result the expression returns.
	 * @param trueCase Expression that is evaluated if the condition is true.
	 * @param falseCase Expression that is evaluated if the condition is false.
	 */
	public SelectExpression(SourceLocation sourceLocation, Expression condition, Expression trueCase, Expression falseCase) {
		super(sourceLocation);
		this.condition = condition;
		this.trueCase = trueCase;
		this.falseCase = falseCase;
	}
	
	@Override public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitSelectExpression(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		SelectExpression that = (SelectExpression) o;
		return Objects.equals(condition, that.condition) &&
				Objects.equals(trueCase, that.trueCase) &&
				Objects.equals(falseCase, that.falseCase);
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), condition, trueCase, falseCase);
	}
}
