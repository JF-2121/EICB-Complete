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

/**
 * AST node representing a variable declaration.
 */
public class VariableDeclaration extends Declaration {
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param typeSpecifier Type of the declared variable.
	 * @param variableName Name of the declared variable.
	 */
	public VariableDeclaration(SourceLocation sourceLocation, TypeSpecifier<?> typeSpecifier, String variableName) {
		super(sourceLocation, typeSpecifier, variableName);
	}
	
	@Override
	public boolean isVariable() {
		return true;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitVariableDeclaration(this, obj);
	}
}
