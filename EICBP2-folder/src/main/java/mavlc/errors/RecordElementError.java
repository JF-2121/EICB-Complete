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
 * Error class to signal an error with an element of a record.
 */
public class RecordElementError extends CompilationError {
	
	private static final long serialVersionUID = -4458464096118608989L;
	
	/**
	 * @param errorOccurrence The AST node where the error occurred.
	 * @param name The name of the record.
	 * @param elementName The name of the element.
	 */
	public RecordElementError(AstNode errorOccurrence, String name, String elementName) {
		message = format("\nInvalid member '%s' of record '%s':\n" +
						"\nFaulty node in %s:\n%s\n",
				elementName, name,
				errorOccurrence.sourceLocation, highlight(errorOccurrence.dump())
		);
	}
}
