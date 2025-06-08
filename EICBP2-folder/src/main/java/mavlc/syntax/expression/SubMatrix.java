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

import java.util.Objects;

/**
 * AST node representing the selection of a sub-matrix from a matrix.
 */
public class SubMatrix extends Expression {
	
	public final Expression structExpression;
	public final Expression rowBaseIndexExpression;
	public final Expression rowStartOffsetExpression;
	public final Expression rowEndOffsetExpression;
	public final Expression colBaseIndexExpression;
	public final Expression colStartOffsetExpression;
	public final Expression colEndOffsetExpression;
	
	protected Integer rowStartOffset;
	protected Integer rowEndOffset;
	protected Integer colStartOffset;
	protected Integer colEndOffset;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param struct The underlying matrix.
	 * @param rowBaseIndex The base row index.
	 * @param rowStartOffset Start offset relative to {@code rowBaseIndex}
	 * @param rowEndOffset End offset relative to {@code rowBaseIndex}
	 * @param colBaseIndex The base column index.
	 * @param colStartOffset Start offset relative to {@code colBaseIndex}
	 * @param colEndOffset End offset relative to {@code colBaseIndex}
	 */
	public SubMatrix(SourceLocation sourceLocation, Expression struct,
	                 Expression rowBaseIndex, Expression rowStartOffset, Expression rowEndOffset,
	                 Expression colBaseIndex, Expression colStartOffset, Expression colEndOffset) {
		super(sourceLocation);
		this.structExpression = struct;
		rowBaseIndexExpression = rowBaseIndex;
		rowStartOffsetExpression = rowStartOffset;
		rowEndOffsetExpression = rowEndOffset;
		colBaseIndexExpression = colBaseIndex;
		colStartOffsetExpression = colStartOffset;
		colEndOffsetExpression = colEndOffset;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitSubMatrix(this, obj);
	}
	
	/**
	 * @return The row start offset.
	 * @throws InternalCompilerError If the row start offset has not been set by the context analysis.
	 */
	public int getRowStartOffset() {
		if(rowStartOffset == null)
			throw new InternalCompilerError("Row start offset of submatrix expression has not been set");
		return rowStartOffset;
	}
	
	public void setRowStartOffset(int startOffset) {
		this.rowStartOffset = startOffset;
	}
	
	public boolean isRowStartOffsetSet() {
		return rowStartOffset != null;
	}
	
	/**
	 * @return The row end offset.
	 * @throws InternalCompilerError If the row end offset has not been set by the context analysis.
	 */
	public int getRowEndOffset() {
		if(rowEndOffset == null)
			throw new InternalCompilerError("Row end offset of submatrix expression has not been set");
		return rowEndOffset;
	}
	
	public void setRowEndOffset(int rowEndOffset) {
		this.rowEndOffset = rowEndOffset;
	}
	
	public boolean isRowEndOffsetSet() {
		return rowEndOffset != null;
	}
	
	/**
	 * @return The column start offset.
	 * @throws InternalCompilerError If the column start offset has not been set by the context analysis.
	 */
	public int getColStartOffset() {
		if(colStartOffset == null)
			throw new InternalCompilerError("Col start offset of submatrix expression has not been set");
		return colStartOffset;
	}
	
	public void setColStartOffset(int startOffset) {
		this.colStartOffset = startOffset;
	}
	
	public boolean isColStartOffsetSet() {
		return colStartOffset != null;
	}
	
	/**
	 * @return The column end offset.
	 * @throws InternalCompilerError If the column end offset has not been set by the context analysis.
	 */
	public int getColEndOffset() {
		if(colEndOffset == null)
			throw new InternalCompilerError("Col end offset of submatrix expression has not been set");
		return colEndOffset;
	}
	
	public void setColEndOffset(int colEndOffset) {
		this.colEndOffset = colEndOffset;
	}
	
	public boolean isColEndOffsetSet() {
		return colEndOffset != null;
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		SubMatrix subMatrix = (SubMatrix) o;
		return Objects.equals(structExpression, subMatrix.structExpression) &&
				Objects.equals(rowBaseIndexExpression, subMatrix.rowBaseIndexExpression) &&
				Objects.equals(rowStartOffsetExpression, subMatrix.rowStartOffsetExpression) &&
				Objects.equals(rowEndOffsetExpression, subMatrix.rowEndOffsetExpression) &&
				Objects.equals(colBaseIndexExpression, subMatrix.colBaseIndexExpression) &&
				Objects.equals(colStartOffsetExpression, subMatrix.colStartOffsetExpression) &&
				Objects.equals(colEndOffsetExpression, subMatrix.colEndOffsetExpression) &&
				Objects.equals(rowStartOffset, subMatrix.rowStartOffset) &&
				Objects.equals(rowEndOffset, subMatrix.rowEndOffset) &&
				Objects.equals(colStartOffset, subMatrix.colStartOffset) &&
				Objects.equals(colEndOffset, subMatrix.colEndOffset);
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), structExpression,
				rowBaseIndexExpression, rowStartOffsetExpression, rowEndOffsetExpression,
				colBaseIndexExpression, colStartOffsetExpression, colEndOffsetExpression);
	}
}
