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
package mavlc.syntax.statement;

import mavlc.errors.InternalCompilerError;
import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.expression.Expression;

import java.util.Objects;

/**
 * AST node representing a case.
 */
public class Case extends SwitchSection {
	
	public final Expression conditionExpression;
	
	protected Integer condition;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param condition The constant expression whose value is compared against the
	 * 		surrounding switch statement's test expression.
	 * @param statement The statement that is executed if the condition is equal to the test expression's value.
	 */
	public Case(SourceLocation sourceLocation, Expression condition, Statement statement) {
		super(sourceLocation, statement);
		this.conditionExpression = condition;
	}
	
	/**
	 * Supply the value of the condition expression after constant evaluation.
	 *
	 * @param condition The value of the condition.
	 */
	public void setCondition(int condition) {
		this.condition = condition;
	}
	
	/**
	 * Get the condition.
	 *
	 * @return The integer constant associated with this case.
	 * @throws InternalCompilerError if the condition expression has not been evaluated yet
	 */
	public int getCondition() {
		if(condition == null) throw new InternalCompilerError("Condition of case has not been evaluated");
		return condition;
	}
	
	public boolean isConditionSet() {
		return condition != null;
	}
	
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitCase(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		Case aCase = (Case) o;
		return Objects.equals(conditionExpression, aCase.conditionExpression) &&
				Objects.equals(condition, aCase.condition);
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), conditionExpression);
	}
}
