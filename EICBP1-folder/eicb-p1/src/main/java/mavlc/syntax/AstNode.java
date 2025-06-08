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
package mavlc.syntax;

import mavlc.services.visualization.Dumper;

/** Abstract super type of all AST nodes. */
public abstract class AstNode {
	public final SourceLocation sourceLocation;
	
	/** @param sourceLocation Location of the node within the containing source file. */
	public AstNode(SourceLocation sourceLocation) {
		this.sourceLocation = sourceLocation;
	}
	
	/**
	 * Accepts the given visitor and calls the corresponding visit-method in the visitor.
	 *
	 * @param visitor Instance of {@link AstNodeVisitor}.
	 * @param obj Additional argument, passed on to the visitor.
	 * @param <RetTy> Return type used by visitor methods.
	 * @param <ArgTy> Argument type used by visitor methods.
	 * @return Return value from the visit-method in the visitor.
	 */
	public abstract <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj);
	
	/**
	 * Accepts the given visitor and calls the corresponding visit-method in the visitor.
	 * This overload will pass null as the additional argument to the visitor.
	 *
	 * @param visitor Instance of {@link AstNodeVisitor}.
	 * @param <RetTy> Return type used by visitor methods.
	 * @param <ArgTy> Argument type used by visitor methods.
	 * @return Return value from the visit-method in the visitor.
	 */
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor) {
		return accept(visitor, null);
	}
	
	public final String dump() {
		return Dumper.dump(this);
	}
}
