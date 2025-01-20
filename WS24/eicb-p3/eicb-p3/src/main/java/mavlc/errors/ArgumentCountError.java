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

import mavlc.syntax.expression.CallExpression;
import mavlc.syntax.function.Function;

import static java.lang.String.format;
import static mavlc.errors.FormattingHelpers.highlight;

/**
 * Error class to signal a mismatch in the argument count
 * between function definition and call expression.
 */
public class ArgumentCountError extends CompilationError {
	
	private static final long serialVersionUID = -4440756150145777220L;
	
	/**
	 * @param call The function call.
	 * @param callee The definition of the called function.
	 * @param expectedCount The number of arguments expected.
	 * @param actualCount The number of arguments given.
	 */
	public ArgumentCountError(CallExpression call, Function callee, int expectedCount, int actualCount) {
		message = format("\nInvalid function call in %s:\n" +
						"> %s arguments specified; expected %d, got %d\n" +
						"\nFaulty node:\n%s\n" +
						"\nFunction signature:\n%s\n",
				call.sourceLocation, (actualCount > expectedCount ? "Too many" : "Not enough"), expectedCount,
				actualCount, highlight(call.dump()), highlight(callee.getSignature())
		);
	}
}
