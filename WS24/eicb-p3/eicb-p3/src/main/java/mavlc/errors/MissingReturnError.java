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

import mavlc.syntax.function.Function;

import static java.lang.String.format;
import static mavlc.errors.FormattingHelpers.highlight;

/**
 * Error class to signal a missing return statement for a non-void
 * function. For non-void functions, the last statement in the
 * function must be a return statement.
 */
public class MissingReturnError extends CompilationError {
	
	private static final long serialVersionUID = 7185478732227926432L;
	
	/**
	 * @param function Non-void function with missing return statement.
	 */
	public MissingReturnError(Function function) {
		message = format("\nMissing return statement for value-type function %s:\n" +
						"\nFunction signature declared in %s:\n%s\n",
				function.name,
				function.sourceLocation,
				highlight(function.getSignature())
		);
	}
}
