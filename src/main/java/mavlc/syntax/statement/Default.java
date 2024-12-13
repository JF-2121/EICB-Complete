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

/**
 * AST node representing a default-case.
 */
public class Default extends SwitchSection {
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param statement The statement that gets executed.
	 */
	public Default(SourceLocation sourceLocation, Statement statement) {
		super(sourceLocation, statement);
	}
	
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitDefault(this, obj);
	}
}
