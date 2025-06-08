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
import mavlc.syntax.type.TypeSpecifier;

import java.util.Objects;

/**
 * AST node representing the iterator in a forEach loop
 */
public class IteratorDeclaration extends Declaration {
	
	/**
	 * whether the declared entity is variable, i.e. assignable.
	 */
	protected final boolean isVariable;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param name Name of the formal parameter.
	 * @param typeSpecifier Type of the formal parameter.
	 * @param isVariable whether the declared entity is variable, i.e. assignable.
	 */
	public IteratorDeclaration(SourceLocation sourceLocation, String name, TypeSpecifier<?> typeSpecifier, boolean isVariable) {
		super(sourceLocation, typeSpecifier, name);
		this.isVariable = isVariable;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitIteratorDeclaration(this, obj);
	}
	
	@Override
	public boolean isVariable() {
		return isVariable;
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		IteratorDeclaration that = (IteratorDeclaration) o;
		return isVariable == that.isVariable;
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), isVariable);
	}
}
