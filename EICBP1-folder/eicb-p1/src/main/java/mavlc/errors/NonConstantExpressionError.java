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

import mavlc.syntax.expression.Expression;

import static java.lang.String.format;
import static mavlc.errors.FormattingHelpers.highlight;

/**
 * Error class to signal that an expression that must be constant wasn't actually a constant expression.
 */
public class NonConstantExpressionError extends CompilationError {
	
	private static final long serialVersionUID = -4922616063906149503L;
	
	/**
	 * @param expr The non-constant expr expression.
	 */
	public NonConstantExpressionError(Expression expr) {
		message = format("\nConstant expression expected in %s:\n" +
						"\nFaulty node:\n%s\n",
				expr.sourceLocation,
				highlight(expr.dump())
		);
	}
}
