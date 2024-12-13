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

/**
 * Error class to signal a reference to a non-declared entity.
 */
public class UndeclaredReferenceError extends CompilationError {
	
	private static final long serialVersionUID = -3670236642691408175L;
	
	/**
	 * @param referencedName The name of the referenced entity.
	 */
	public UndeclaredReferenceError(String referencedName) {
		message = "Identifier " + referencedName + " has not been declared!";
	}
}
