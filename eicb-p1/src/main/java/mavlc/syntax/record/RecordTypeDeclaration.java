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

import mavlc.errors.InternalCompilerError;
import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.SourceLocation;

import java.util.List;
import java.util.Objects;

/**
 * AST node representing a record type declaration.
 */
public class RecordTypeDeclaration extends AstNode {
	
	public final String name;
	public final List<RecordElementDeclaration> elements;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param name Name of the declared entity.
	 * @param elements List of all record element declarations that are part of this record type declaration.
	 */
	public RecordTypeDeclaration(SourceLocation sourceLocation, String name, List<RecordElementDeclaration> elements) {
		super(sourceLocation);
		this.name = name;
		this.elements = elements;
	}
	
	/**
	 * Get the element with a specified name.
	 *
	 * @param name The specified name.
	 * @return The element with the specified name
	 * 		null, if name is null or no such element exists.
	 */
	public RecordElementDeclaration getElement(String name) {
		if(name == null) return null;
		for(RecordElementDeclaration element : elements) {
			if(name.equals(element.name)) {
				return element;
			}
		}
		return null;
	}
	
	/**
	 * @param name Name of the element.
	 * @return Offset of the element.
	 * @throws InternalCompilerError If the element does not exist.
	 */
	public int getElementOffset(String name) {
		int offset = 0;
		for(RecordElementDeclaration element : elements) {
			if(element.name.equals(name)) {
				return offset;
			}
			offset += element.getType().wordSize;
		}
		throw new InternalCompilerError("Element '" + name + "' does not exist");
	}
	
	@Override
	public String toString() {
		return "record " + name;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitRecordTypeDeclaration(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		RecordTypeDeclaration that = (RecordTypeDeclaration) o;
		return Objects.equals(name, that.name) &&
				Objects.equals(elements, that.elements);
	}
	
	@Override public int hashCode() {
		return Objects.hash(name, elements);
	}
}
