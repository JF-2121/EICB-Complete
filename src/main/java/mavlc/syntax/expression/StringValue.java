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
import mavlc.type.StringType;
import mavlc.type.Type;

/**
 * AST node representing a string literal.
 */
public class StringValue extends Expression {
	
	public final String value;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param value The text of this string value.
	 */
	public StringValue(SourceLocation sourceLocation, String value) {
		super(sourceLocation);
		type = StringType.instance;
		this.value = value;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitStringValue(this, obj);
	}
	
	@Override
	public void setType(Type type) {
		if(!StringType.instance.equals(type)) {
			throw new UnsupportedOperationException("Cannot set another type than StringType for StringValue!");
		}
	}
	
	public String getEscapedValue() {
		return '"' + value
				.replace("\\", "\\\\")
				.replace("\"", "\\\"")
				.replace("\n", "\\n")
				.replace("\r", "\\r")
				.replace("\t", "\\t") + '"';
	}
}
