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
 * AST node representing a foreach loop.
 */
public class ForEachLoop extends Statement {
	
	public final IteratorDeclaration iteratorDeclaration;
	public final Expression structExpression;
	public final Statement body;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param iterator The iterator used for iterating the struct.
	 * @param struct The expression that represents the struct.
	 * @param body The loop body.
	 */
	public ForEachLoop(SourceLocation sourceLocation, IteratorDeclaration iterator, Expression struct,
	                   Statement body) {
		super(sourceLocation);
		this.iteratorDeclaration = iterator;
		this.structExpression = struct;
		this.body = body;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitForEachLoop(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		ForEachLoop that = (ForEachLoop) o;
		return Objects.equals(iteratorDeclaration, that.iteratorDeclaration) &&
				Objects.equals(structExpression, that.structExpression) &&
				Objects.equals(body, that.body);
	}
	
	@Override public int hashCode() {
		return Objects.hash(iteratorDeclaration, structExpression, body);
	}
}
