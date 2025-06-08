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

import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.expression.Expression;

import java.util.Objects;

/**
 * AST node representing a MAVL if statement.
 */
public class IfStatement extends Statement {
	
	public final Expression condition;
	public final Statement thenStatement;
	public final Statement elseStatement;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param condition Test expression.
	 * @param thenStatement Statement for the true-case.
	 */
	public IfStatement(SourceLocation sourceLocation, Expression condition, Statement thenStatement) {
		super(sourceLocation);
		this.condition = condition;
		this.thenStatement = thenStatement;
		this.elseStatement = null;
	}
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param condition Test-expression.
	 * @param thenStatement Statement for the true-case.
	 * @param elseStatement Statement for the false-case.
	 */
	public IfStatement(SourceLocation sourceLocation, Expression condition, Statement thenStatement, Statement elseStatement) {
		super(sourceLocation);
		this.condition = condition;
		this.thenStatement = thenStatement;
		this.elseStatement = elseStatement;
	}
	
	/** @return True if this if statement has an else statement. */
	public boolean hasElseStatement() {
		return elseStatement != null;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitIfStatement(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		IfStatement that = (IfStatement) o;
		return Objects.equals(condition, that.condition) &&
				Objects.equals(thenStatement, that.thenStatement) &&
				Objects.equals(elseStatement, that.elseStatement);
	}
	
	@Override public int hashCode() {
		return Objects.hash(condition, thenStatement, elseStatement);
	}
}
