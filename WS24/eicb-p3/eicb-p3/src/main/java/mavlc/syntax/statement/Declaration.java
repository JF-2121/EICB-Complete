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

import mavlc.errors.InapplicableOperationError;
import mavlc.errors.InternalCompilerError;
import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.HasType;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.type.TypeSpecifier;
import mavlc.type.Type;
import mavlc.type.ValueType;

import java.util.Objects;

/**
 * Abstract super-class of all declaration sites of
 * referenceable entities.
 */
public abstract class Declaration extends Statement implements HasType {
	
	public final TypeSpecifier<?> typeSpecifier;
	public final String name;
	
	protected ValueType type;
	protected Integer localBaseOffset;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param typeSpecifier {@link TypeSpecifier} AST node.
	 * @param name Name of the declared entity.
	 */
	public Declaration(SourceLocation sourceLocation, TypeSpecifier<?> typeSpecifier, String name) {
		super(sourceLocation);
		this.typeSpecifier = typeSpecifier;
		this.name = name;
	}
	
	/**
	 * Get the {@link Type} of the declared entity.
	 *
	 * @return The type of the entity.
	 * @throws InternalCompilerError If the type has not been set yet
	 */
	@Override
	public ValueType getType() {
		if(type == null) throw new InternalCompilerError(this, "Type of declaration has not been set");
		return type;
	}
	
	@Override
	public void setType(Type type) {
		// void is not a valid type for declarations
		if(!type.isValueType()) {
			throw new InapplicableOperationError(this, type, ValueType.class);
		}
		this.type = (ValueType) type;
	}
	
	@Override
	public boolean isTypeSet() {
		return type != null;
	}
	
	/**
	 * Check if the declared entity is variable, i.e. assignable.
	 *
	 * @return True if the entity is assignable.
	 */
	public abstract boolean isVariable();
	
	/**
	 * Get the offset from the local base of the
	 * surrounding frame for the declared entity.
	 *
	 * @return Offset from LB.
	 */
	public int getLocalBaseOffset() {
		if(localBaseOffset == null)
			throw new InternalCompilerError("Local base offset of declaration has not been set.");
		return localBaseOffset;
	}
	
	/**
	 * Set the offset from the local base of the
	 * surrounding frame for the declared entity.
	 *
	 * @param localBaseOffset Offset from LB.
	 */
	public void setLocalBaseOffset(int localBaseOffset) {
		this.localBaseOffset = localBaseOffset;
	}
	
	public boolean isLocalBaseOffsetSet() {
		return localBaseOffset != null;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitDeclaration(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		Declaration that = (Declaration) o;
		return Objects.equals(typeSpecifier, that.typeSpecifier) &&
				Objects.equals(name, that.name) &&
				Objects.equals(type, that.type) &&
				Objects.equals(localBaseOffset, that.localBaseOffset);
	}
	
	@Override public int hashCode() {
		return Objects.hash(typeSpecifier, name);
	}
}
