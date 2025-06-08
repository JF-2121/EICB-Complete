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

import java.util.Objects;

/**
 * AST node representing a record element as left-hand side of an assignment.
 */
public class RecordLhsIdentifier extends LeftHandIdentifier {
	
	public final String elementName;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param variableName Name of the referenced record.
	 * @param elementName Name of the selected element.
	 */
	public RecordLhsIdentifier(SourceLocation sourceLocation, String variableName, String elementName) {
		super(sourceLocation, variableName);
		this.elementName = elementName;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitRecordLhsIdentifier(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		RecordLhsIdentifier that = (RecordLhsIdentifier) o;
		return Objects.equals(elementName, that.elementName);
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), elementName);
	}
}
