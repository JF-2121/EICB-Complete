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
package mavlc.context_analysis;

import mavlc.errors.InternalCompilerError;
import mavlc.syntax.statement.Declaration;

/* TODO enter group information
 *
 * EiCB group number: 149
 * Joshua Liam Friedel (279635)
 * Benedikt Schwarz (2373528)
 * Lasse Ramon Reith (2674146)

/**
 * A table for identifiers used inside a function.
 */
public class IdentificationTable {
	private Scope currentScope;

	/**
	 *  Constructor initializes the current scope as null
	 */
	public IdentificationTable() {
		currentScope = null;
	}
	/**
	 * Declares the given identifier in the current scope.
	 *
	 * @param name the identifier to declare
	 * @param declaration the reference to the identifier's declaration site
	 */
	public void addIdentifier(String name, Declaration declaration) {
		// TODO implement (task 2.1)
		if (currentScope == null) {
			throw new InternalCompilerError("No scope for declaration");
		}
		currentScope.addIdentifier(name, declaration);	}

	/**
	 * Looks up the innermost declaration of the given identifier.
	 *
	 * @param name the identifier to look up
	 * @return the identifier's innermost declaration site
	 */
	public Declaration getDeclaration(String name) {
		// TODO implement (task 2.1)
		if (currentScope == null) {
			throw new InternalCompilerError("No scope for declaration");
		}
		return currentScope.getDeclaration(name);
	}


	/**
	 * Opens a new scope.
	 */
	public void openNewScope() {
		// TODO implement (task 2.1)
		currentScope = new Scope(currentScope);
	}

	/**
	 * Closes the current scope.
	 */
	public void closeCurrentScope() {
		// TODO implement (task 2.1)
		if (currentScope == null) {
			throw new InternalCompilerError("No scope has been opened");
		}
		currentScope = currentScope.parentScope;
	}
}
