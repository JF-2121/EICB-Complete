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

import mavlc.errors.InternalCompilerError;
import mavlc.syntax.AstNode;
import mavlc.syntax.HasType;
import mavlc.syntax.SourceLocation;
import mavlc.type.Type;

public abstract class TypeSpecifier<TypeT extends Type> extends AstNode implements HasType {
	private final Class<TypeT> typeClass;
	private TypeT type;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param typeClass The class of the {@link Type} this specifier represents.
	 */
	protected TypeSpecifier(SourceLocation sourceLocation, Class<TypeT> typeClass) {
		super(sourceLocation);
		this.typeClass = typeClass;
	}
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param typeClass The class of the {@link Type} this specifier represents.
	 * @param type The singleton instance of the {@link Type} this specifier represents.
	 */
	protected TypeSpecifier(SourceLocation sourceLocation, Class<TypeT> typeClass, TypeT type) {
		super(sourceLocation);
		this.typeClass = typeClass;
		this.type = type;
	}
	
	/**
	 * Get the {@link Type} of the declared entity.
	 *
	 * @return The type of the entity.
	 * @throws InternalCompilerError If the type has not been set yet.
	 */
	@Override
	public TypeT getType() {
		if(type == null) throw new InternalCompilerError(this, "Type of type specifier has not been set");
		return type;
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public void setType(Type type) {
		if(type.getClass() != typeClass)
			throw new InternalCompilerError("Cannot set type of " + getClass().getSimpleName() + " to " + type + "; only " + typeClass.getSimpleName() + " allowed");
		this.type = (TypeT) type;
	}
	
	@Override
	public boolean isTypeSet() {
		return type != null;
	}
	
	@Override public String toString() {
		return dump();
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		return o != null && getClass() == o.getClass();
	}
	
	@Override public int hashCode() {
		return 0;
	}
}
