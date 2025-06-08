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
 * AST node representing the selection of a sub-vector from a vector.
 */
public class SubVector extends Expression {
	
	public final Expression structExpression;
	public final Expression baseIndexExpression;
	public final Expression startOffsetExpression;
	public final Expression endOffsetExpression;
	
	protected Integer startOffset;
	protected Integer endOffset;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param struct The underlying vector.
	 * @param baseIndex The base-index.
	 * @param startOffset The start-offset from the base-index.
	 * @param endOffset The end-offset from the base-index.
	 */
	public SubVector(SourceLocation sourceLocation, Expression struct, Expression baseIndex, Expression startOffset, Expression endOffset) {
		super(sourceLocation);
		structExpression = struct;
		baseIndexExpression = baseIndex;
		startOffsetExpression = startOffset;
		endOffsetExpression = endOffset;
	}
	
	/**
	 * @return The start offset.
	 * @throws InternalCompilerError If the start offset has not been set by the context analysis.
	 */
	public int getStartOffset() {
		if(startOffset == null)
			throw new InternalCompilerError("Start offset of subvector expression has not been set");
		return startOffset;
	}
	
	public void setStartOffset(int startOffset) {
		this.startOffset = startOffset;
	}
	
	public boolean isStartOffsetSet() {
		return startOffset != null;
	}
	
	/**
	 * @return The end offset.
	 * @throws InternalCompilerError If the end offset has not been set by the context analysis.
	 */
	public int getEndOffset() {
		if(endOffset == null)
			throw new InternalCompilerError("End offset of subvector expression has not been set");
		return endOffset;
	}
	
	public void setEndOffset(int endOffset) {
		this.endOffset = endOffset;
	}
	
	public boolean isEndOffsetSet() {
		return endOffset != null;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitSubVector(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		SubVector subVector = (SubVector) o;
		return Objects.equals(structExpression, subVector.structExpression) &&
				Objects.equals(baseIndexExpression, subVector.baseIndexExpression) &&
				Objects.equals(startOffsetExpression, subVector.startOffsetExpression) &&
				Objects.equals(endOffsetExpression, subVector.endOffsetExpression) &&
				Objects.equals(startOffset, subVector.startOffset) &&
				Objects.equals(endOffset, subVector.endOffset);
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), structExpression, baseIndexExpression, startOffsetExpression, endOffsetExpression);
	}
}
