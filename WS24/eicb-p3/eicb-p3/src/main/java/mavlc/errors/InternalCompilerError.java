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
package mavlc.errors;

import mavlc.syntax.AstNode;

/** Error class to signal an internal error in the compiler implementation. */
public class InternalCompilerError extends CompilationError {
	private static final long serialVersionUID = 5903172577640483949L;
	
	public final AstNode errorNode;
	
	/**
	 * @param errorNode The AST node where the error occurred.
	 * @param message A message detailing the error.
	 */
	public InternalCompilerError(AstNode errorNode, String message) {
		this.errorNode = errorNode;
		this.message = message + " @ " + errorNode.sourceLocation;
	}
	
	/**
	 * @param message A message detailing the error.
	 */
	public InternalCompilerError(String message) {
		this.errorNode = null;
		this.message = message;
	}
}
