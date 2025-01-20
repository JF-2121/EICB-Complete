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
import mavlc.syntax.SourceLocation;

import static java.lang.String.format;

/** Error class to signal a mismatch between the user generated AST and the reference supplied with a test case. */
public class AstMismatchError extends CompilationError {
	
	private static final long serialVersionUID = 8418447257622383260L;
	
	/**
	 * @param testedNode The ast node compared to the reference.
	 * @param referenceNode The reference ast node.
	 * @param message A message detailing the mismatch.
	 */
	public AstMismatchError(AstNode testedNode, AstNode referenceNode, String message) {
		if(testedNode != null) {
			this.message = format("\nAst mismatch in %s:\n" +
							"> %s\n" +
							"\nTested node:\n%s\n" +
							"\nReference node:\n%s\n",
					testedNode.sourceLocation, message, testedNode.dump(), referenceNode.dump()
			);
		}
		else {
			this.message = format("\nAst mismatch in %s:\n" +
							"> %s\n" +
							"\nExpected node:\n%s\n",
					SourceLocation.unknown, message, referenceNode.dump()
			);
		}
	}
}
