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

import static java.lang.Math.max;
import static java.lang.Math.min;
import static java.lang.String.format;
import static mavlc.errors.FormattingHelpers.highlight;

/**
 * Error class to signal a mismatch in the dimensions of a matrix, vector or record.
 */
public class StructureDimensionError extends CompilationError {
	
	private static final long serialVersionUID = -840530908555961258L;
	
	/**
	 * @param node AST node where the error occurred.
	 * @param dimension1 The first of the compared dimensions.
	 * @param dimension2 The second of the compared dimensions.
	 */
	public StructureDimensionError(AstNode node, int dimension1, int dimension2) {
		message = format("\nMismatching dimensions %d and %d:\n" +
						"\nFaulty node in %s:\n%s\n",
				min(dimension1, dimension2), max(dimension1, dimension2),
				node.sourceLocation, highlight(node.dump())
		);
	}
	
	/**
	 * @param node AST node where the error occurred.
	 * @param error The error message.
	 */
	public StructureDimensionError(AstNode node, String error) {
		message = format("\n%s:\n" +
						"\nFaulty node in %s:\n%s\n",
				error, node.sourceLocation, highlight(node.dump())
		);
	}
}
