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

import mavlc.syntax.statement.SwitchSection;
import mavlc.syntax.statement.SwitchStatement;

import static java.lang.String.format;
import static mavlc.errors.FormattingHelpers.highlight;

/**
 * Error class to signal duplicate (default) cases in a SwitchStmt.
 */
public class DuplicateCaseError extends CompilationError {
	private static final long serialVersionUID = 4542147676795931253L;
	
	/**
	 * @param switchStmt the surrounding switch statement.
	 * @param isDefaultCase true if multiple default cases were found, false otherwise (non-unique cases).
	 * @param case1 the first affected (default) case.
	 * @param case2 the second affected (default) case.
	 */
	public DuplicateCaseError(SwitchStatement switchStmt, boolean isDefaultCase, SwitchSection case1, SwitchSection case2) {
		message = format("\nInvalid switch statement in %s:\n" +
						"> %s were found.\n" +
						"\nFaulty node:\n%s\n" +
						"\nFirst case:\n%s\n" +
						"\nSecond case:\n%s\n",
				switchStmt.sourceLocation,
				(isDefaultCase ? "Multiple default cases" : "Two cases with the same condition"),
				highlight(switchStmt.dump()), highlight(case1.dump()), highlight(case2.dump())
		);
	}
}
