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
 * AST node representing the selection of a single element from a matrix or vector.
 */
public class ElementSelect extends Expression {
	
	public final Expression structExpression;
	public final Expression indexExpression;
	
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param struct A matrix or vector.
	 * @param index The index of the chosen element.
	 */
	public ElementSelect(SourceLocation sourceLocation, Expression struct, Expression index) {
		super(sourceLocation);
		this.structExpression = struct;
		this.indexExpression = index;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitElementSelect(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		ElementSelect that = (ElementSelect) o;
		return Objects.equals(structExpression, that.structExpression) &&
				Objects.equals(indexExpression, that.indexExpression);
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), structExpression, indexExpression);
	}
}
