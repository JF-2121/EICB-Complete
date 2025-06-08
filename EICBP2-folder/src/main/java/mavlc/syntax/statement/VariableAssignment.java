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
 * AST node representing an assignment of a variable.
 */
public class VariableAssignment extends Statement {
	
	public final LeftHandIdentifier identifier;
	public final Expression value;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param identifier The left-hand side of the assignment.
	 * @param value The assigned value.
	 */
	public VariableAssignment(SourceLocation sourceLocation, LeftHandIdentifier identifier, Expression value) {
		super(sourceLocation);
		this.identifier = identifier;
		this.value = value;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitVariableAssignment(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		VariableAssignment that = (VariableAssignment) o;
		return Objects.equals(identifier, that.identifier) &&
				Objects.equals(value, that.value);
	}
	
	@Override public int hashCode() {
		return Objects.hash(identifier, value);
	}
}
