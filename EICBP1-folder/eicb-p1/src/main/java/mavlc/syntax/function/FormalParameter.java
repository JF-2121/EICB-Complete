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
package mavlc.syntax.function;

import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.statement.Declaration;
import mavlc.syntax.type.TypeSpecifier;

/**
 * AST node representing a formal parameter in a function definition.
 */
public class FormalParameter extends Declaration {
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param name Name of the formal parameter.
	 * @param typeSpecifier Type specifier of the formal parameter.
	 */
	public FormalParameter(SourceLocation sourceLocation, String name, TypeSpecifier<?> typeSpecifier) {
		super(sourceLocation, typeSpecifier, name);
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitFormalParameter(this, obj);
	}
	
	@Override
	public boolean isVariable() {
		return true;
	}
}
