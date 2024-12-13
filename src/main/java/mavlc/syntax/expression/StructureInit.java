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

import java.util.List;
import java.util.Objects;

/**
 * AST node representing the construction of a matrix or vector from
 * the underlying values.
 */
public class StructureInit extends Expression {
	
	public final List<Expression> elements;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param elements A list of expressions representing the structure elements.
	 */
	public StructureInit(SourceLocation sourceLocation, List<Expression> elements) {
		super(sourceLocation);
		this.elements = elements;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitStructureInit(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		StructureInit that = (StructureInit) o;
		return Objects.equals(elements, that.elements);
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), elements);
	}
}
