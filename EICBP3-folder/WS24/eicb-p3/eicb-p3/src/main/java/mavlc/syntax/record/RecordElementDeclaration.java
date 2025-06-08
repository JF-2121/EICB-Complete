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
package mavlc.syntax.record;

import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.statement.Declaration;
import mavlc.syntax.type.TypeSpecifier;

import java.util.Objects;

/**
 * AST node representing a record element declaration.
 */
public class RecordElementDeclaration extends Declaration {
	
	protected final boolean isVariable;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param isVariable Whether the declared entity is variable.
	 * @param typeSpecifier {@link TypeSpecifier} of the declared entity.
	 * @param name Name of the declared entity.
	 */
	public RecordElementDeclaration(SourceLocation sourceLocation, boolean isVariable, TypeSpecifier<?> typeSpecifier, String name) {
		super(sourceLocation, typeSpecifier, name);
		this.isVariable = isVariable;
	}
	
	@Override
	public boolean isVariable() {
		return isVariable;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitRecordElementDeclaration(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		RecordElementDeclaration that = (RecordElementDeclaration) o;
		return isVariable == that.isVariable;
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), isVariable);
	}
}
