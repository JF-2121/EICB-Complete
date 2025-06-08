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

import static java.lang.String.format;
import static mavlc.errors.FormattingHelpers.highlight;

/**
 * Error class to signal a misplaced return statement. A return
 * statement can be misplaced if it occurs in any other position
 * than the last in a non-void function.
 */
public class MisplacedReturnError extends CompilationError {
	
	private static final long serialVersionUID = -1119302498976342047L;
	
	/**
	 * @param returnStmt The misplaced return statement.
	 */
	public MisplacedReturnError(AstNode returnStmt) {
		message = format("\nMisplaced return statement in %s:\n" +
						"\nFaulty node:\n%s\n",
				returnStmt.sourceLocation,
				highlight(returnStmt.dump())
		);
	}
}
