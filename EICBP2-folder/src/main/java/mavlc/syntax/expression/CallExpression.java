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
import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.function.Function;

import java.util.List;
import java.util.Objects;


/**
 * AST node representing a function-call expression.
 */
public class CallExpression extends Expression {
	
	public final String functionName;
	public final List<Expression> actualParameters;
	
	protected Function callee;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param functionName Name of the called {@link Function}.
	 * @param actualParameters A list of expressions representing the actual parameters of this call.
	 */
	public CallExpression(SourceLocation sourceLocation, String functionName, List<Expression> actualParameters) {
		super(sourceLocation);
		this.functionName = functionName;
		this.actualParameters = actualParameters;
	}
	
	/**
	 * Get the defining AST node of the called function.
	 *
	 * @return The callee definition
	 */
	public Function getCalleeDefinition() {
		if(callee == null) throw new InternalCompilerError("Callee definition of call expression has not been set");
		return callee;
	}
	
	/**
	 * Set the defining AST node of the called function.
	 *
	 * @param calleeDefinition The callee definition
	 */
	public void setCalleeDefinition(Function calleeDefinition) {
		this.callee = calleeDefinition;
	}
	
	public boolean isCalleeDefinitionSet() {
		return callee != null;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitCallExpression(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		CallExpression that = (CallExpression) o;
		return Objects.equals(functionName, that.functionName) &&
				Objects.equals(actualParameters, that.actualParameters) &&
				callee == that.callee;
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), functionName, actualParameters);
	}
}
