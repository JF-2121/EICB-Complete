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

import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.SourceLocation;

/**
 * Abstract super class of all MAVL program-statements.
 */
public abstract class Statement extends AstNode {
	/** @param sourceLocation The location in which the node was specified. */
	public Statement(SourceLocation sourceLocation) { super(sourceLocation); }
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitStatement(this, obj);
	}
}
