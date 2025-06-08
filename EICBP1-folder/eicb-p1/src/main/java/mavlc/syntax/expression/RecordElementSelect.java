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

import java.util.Objects;

/**
 * AST node representing the selection of an element from a record.
 */
public class RecordElementSelect extends Expression {
	
	public final Expression recordExpression;
	public final String elementName;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param record A record.
	 * @param elementName The name of the chosen element.
	 */
	public RecordElementSelect(SourceLocation sourceLocation, Expression record, String elementName) {
		super(sourceLocation);
		this.recordExpression = record;
		this.elementName = elementName;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitRecordElementSelect(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		RecordElementSelect that = (RecordElementSelect) o;
		return Objects.equals(recordExpression, that.recordExpression) &&
				Objects.equals(elementName, that.elementName);
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), recordExpression, elementName);
	}
}
