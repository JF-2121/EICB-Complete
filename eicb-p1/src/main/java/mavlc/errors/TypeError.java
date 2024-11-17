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
import mavlc.type.Type;

import static java.lang.String.format;
import static mavlc.errors.FormattingHelpers.highlight;

/**
 * Error class to signal a type mismatch.
 */
public class TypeError extends CompilationError {
	
	private static final long serialVersionUID = -7109237710454912082L;
	
	/**
	 * @param faultyNode The AST node where the error occurred.
	 * @param type1 The first compared type.
	 * @param type2 The second compared type.
	 */
	public TypeError(AstNode faultyNode, Type type1, Type type2) {
		if(type1.toString().compareTo(type2.toString()) > 0) {
			Type t = type1;
			type1 = type2;
			type2 = t;
		}
		
		message = format("\nMismatching types '%s' and '%s':\n" +
						"\nFaulty node in %s:\n%s\n",
				type1, type2, faultyNode.sourceLocation, highlight(faultyNode.dump())
		);
	}
}
