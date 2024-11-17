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
import mavlc.syntax.function.Function;

import static java.lang.String.format;
import static mavlc.errors.FormattingHelpers.highlight;

/**
 * Error class to signal a non-permitted overwriting of an already
 * existing declaration.
 */
public class OverwritingDeclarationError extends CompilationError {
	
	private static final long serialVersionUID = 7924223292324293288L;
	
	/**
	 * @param name Name of the declared entity.
	 * @param prevDecl The previously existing declaration.
	 * @param newDecl The second declaration.
	 */
	public OverwritingDeclarationError(String name, AstNode prevDecl, AstNode newDecl) {
		message = format("\nIdentifier '%s' has already been declared in the same scope:\n" +
						"\nOriginal declaration in %s:\n%s\n" +
						"\nDuplicate declaration in %s:\n%s\n",
				name,
				prevDecl.sourceLocation, highlight(prevDecl instanceof Function ? ((Function) prevDecl).getSignature() : prevDecl.dump()),
				newDecl.sourceLocation, highlight(newDecl instanceof Function ? ((Function) newDecl).getSignature() : newDecl.dump())
		);
	}
}
