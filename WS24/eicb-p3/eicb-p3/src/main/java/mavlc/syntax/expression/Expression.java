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

import mavlc.errors.InternalCompilerError;
import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.HasType;
import mavlc.syntax.SourceLocation;
import mavlc.type.Type;

import java.util.Objects;

/**
 * Abstract super type of all AST nodes representing expressions.
 */
public abstract class Expression extends AstNode implements HasType {
	
	protected Type type;
	
	/** @param sourceLocation The location in which the node was specified. */
	public Expression(SourceLocation sourceLocation) {
		super(sourceLocation);
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitExpression(this, obj);
	}
	
	/**
	 * Get the {@link mavlc.type.Type} of this expression.
	 *
	 * @return The {@link mavlc.type.Type} of this expression.
	 */
	@Override
	public Type getType() {
		if(type == null) throw new InternalCompilerError("Type of expression has not been set");
		return type;
	}
	
	/**
	 * Set the {@link mavlc.type.Type} of this expression.
	 *
	 * @param type The {@link mavlc.type.Type} of this expression.
	 */
	@Override
	public void setType(Type type) {
		this.type = type;
	}
	
	@Override public boolean isTypeSet() {
		return type != null;
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		Expression that = (Expression) o;
		return Objects.equals(type, that.type);
	}
	
	@Override
	public int hashCode() {
		return 0;
	}
}
