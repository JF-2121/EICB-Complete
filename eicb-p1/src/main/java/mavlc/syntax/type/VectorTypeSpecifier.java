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
package mavlc.syntax.type;

import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.expression.Expression;
import mavlc.type.VectorType;

import java.util.Objects;

public class VectorTypeSpecifier extends TypeSpecifier<VectorType> {
	public final TypeSpecifier<?> elementTypeSpecifier;
	public final Expression dimensionExpression;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param elementTypeSpecifier Type specifier denoting the element type.
	 * @param dimensionExpression Expression which evaluates to the vector dimension.
	 */
	public VectorTypeSpecifier(SourceLocation sourceLocation, TypeSpecifier<?> elementTypeSpecifier, Expression dimensionExpression) {
		super(sourceLocation, VectorType.class);
		this.elementTypeSpecifier = elementTypeSpecifier;
		this.dimensionExpression = dimensionExpression;
	}
	
	@Override public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitVectorTypeSpecifier(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		VectorTypeSpecifier that = (VectorTypeSpecifier) o;
		return Objects.equals(elementTypeSpecifier, that.elementTypeSpecifier) &&
				Objects.equals(dimensionExpression, that.dimensionExpression);
	}
	
	@Override public int hashCode() {
		return Objects.hash(elementTypeSpecifier, dimensionExpression);
	}
}
