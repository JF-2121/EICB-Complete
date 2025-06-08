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
import mavlc.syntax.statement.Declaration;

import static java.lang.String.format;
import static mavlc.errors.FormattingHelpers.highlight;

/**
 * Error class to signal a non-permitted assignment to a constant value.
 */
public class ConstantAssignmentError extends CompilationError {
	
	private static final long serialVersionUID = 4663714320475566108L;
	
	/**
	 * @param node LhsIdentifier of a faulty assignment or the ForLoop node.
	 * @param declaration Declaration of the assigned identifier.
	 */
	public ConstantAssignmentError(AstNode node, Declaration declaration) {
		if(declaration != null) {
			message = format("\nInvalid assignment in %s:\n" +
							"> Cannot assign constant identifier '%s'.\n" +
							"\nFaulty node:\n%s\n" +
							"\nIdentifier declaration in %s:\n%s\n",
					node.sourceLocation, declaration.name, highlight(node.dump()),
					declaration.sourceLocation, highlight(declaration.dump())
			);
		} else {
			message = format("\nInvalid assignment in %s:\n" +
							"> Cannot assign expression.\n" +
							"\nFaulty node:\n%s\n",
					node.sourceLocation, highlight(node.dump())
			);
		}
	}
}
