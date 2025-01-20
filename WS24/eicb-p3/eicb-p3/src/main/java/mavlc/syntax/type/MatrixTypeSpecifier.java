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
import mavlc.type.MatrixType;

import java.util.Objects;

public class MatrixTypeSpecifier extends TypeSpecifier<MatrixType> {
	public final TypeSpecifier<?> elementTypeSpecifier;
	public final Expression rowsExpression;
	public final Expression colsExpression;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param elementTypeSpecifier Type specifier denoting the element type.
	 * @param rowsExpression Expression which evaluates to the first matrix dimension.
	 * @param colsExpression Expression which evaluates to the second matrix dimension.
	 */
	public MatrixTypeSpecifier(SourceLocation sourceLocation, TypeSpecifier<?> elementTypeSpecifier, Expression rowsExpression, Expression colsExpression) {
		super(sourceLocation, MatrixType.class);
		this.elementTypeSpecifier = elementTypeSpecifier;
		this.rowsExpression = rowsExpression;
		this.colsExpression = colsExpression;
	}
	
	@Override public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitMatrixTypeSpecifier(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		MatrixTypeSpecifier that = (MatrixTypeSpecifier) o;
		return Objects.equals(elementTypeSpecifier, that.elementTypeSpecifier) &&
				Objects.equals(rowsExpression, that.rowsExpression) &&
				Objects.equals(colsExpression, that.colsExpression);
	}
	
	@Override public int hashCode() {
		return Objects.hash(elementTypeSpecifier, rowsExpression, colsExpression);
	}
}
