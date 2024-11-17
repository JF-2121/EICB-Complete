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
 * AST node representing a vector element as left-hand side of an assignment.
 */
public class VectorLhsIdentifier extends LeftHandIdentifier {
	
	public final Expression indexExpression;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param name Name of the referenced vector.
	 * @param index Index of the referenced element.
	 */
	public VectorLhsIdentifier(SourceLocation sourceLocation, String name, Expression index) {
		super(sourceLocation, name);
		this.indexExpression = index;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitVectorLhsIdentifier(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		VectorLhsIdentifier that = (VectorLhsIdentifier) o;
		return Objects.equals(indexExpression, that.indexExpression);
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), indexExpression);
	}
}
