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
 * AST node representing a for loop.
 */
public class ForLoop extends Statement {
	
	public final String initVarName;
	public final Expression initExpression;
	public final Expression loopCondition;
	public final String incrVarName;
	public final Expression incrExpression;
	public final Statement body;
	
	protected Declaration initVarDecl;
	protected Declaration incrVarDecl;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param initVarName Name of the initialized variable.
	 * @param initExpression Initialized value.
	 * @param loopCondition The loop condition.
	 * @param incrVarName The name of the incremented variable.
	 * @param incrExpression The increment expression.
	 * @param body The loop body.
	 */
	public ForLoop(SourceLocation sourceLocation,
	               String initVarName, Expression initExpression, Expression loopCondition,
	               String incrVarName, Expression incrExpression, Statement body) {
		super(sourceLocation);
		this.initVarName = initVarName;
		this.initExpression = initExpression;
		this.loopCondition = loopCondition;
		this.incrVarName = incrVarName;
		this.incrExpression = incrExpression;
		this.body = body;
	}
	
	public Declaration getInitVarDeclaration() {
		if(initVarDecl == null)
			throw new InternalCompilerError("Init var declaration of for loop has not been set");
		return initVarDecl;
	}
	
	public void setInitVarDeclaration(Declaration initVarDecl) {
		this.initVarDecl = initVarDecl;
	}
	
	public boolean isInitVarDeclarationSet() {
		return initVarDecl != null;
	}
	
	public Declaration getIncrVarDeclaration() {
		if(incrVarDecl == null)
			throw new InternalCompilerError("Increment var declaration of for loop has not been set");
		return incrVarDecl;
	}
	
	public void setIncrVarDeclaration(Declaration incrVarDecl) {
		this.incrVarDecl = incrVarDecl;
	}
	
	public boolean isIncrVarDeclarationSet() {
		return incrVarDecl != null;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitForLoop(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		ForLoop forLoop = (ForLoop) o;
		return Objects.equals(initVarName, forLoop.initVarName) &&
				Objects.equals(initExpression, forLoop.initExpression) &&
				Objects.equals(loopCondition, forLoop.loopCondition) &&
				Objects.equals(incrVarName, forLoop.incrVarName) &&
				Objects.equals(incrExpression, forLoop.incrExpression) &&
				Objects.equals(body, forLoop.body) &&
				Objects.equals(initVarDecl, forLoop.initVarDecl) &&
				Objects.equals(incrVarDecl, forLoop.incrVarDecl);
	}
	
	@Override public int hashCode() {
		return Objects.hash(initVarName, initExpression, loopCondition, incrVarName, incrExpression, body);
	}
}
