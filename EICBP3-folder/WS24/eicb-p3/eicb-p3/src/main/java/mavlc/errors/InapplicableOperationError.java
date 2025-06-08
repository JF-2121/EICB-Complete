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
import mavlc.type.Type;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.stream.Collectors;

import static java.lang.String.format;
import static mavlc.errors.FormattingHelpers.highlight;

/**
 * Error class to signal that an operation is not applicable to
 * the type of the underlying element.
 */
public class InapplicableOperationError extends CompilationError {
	
	private static final long serialVersionUID = 71536221645989346L;
	
	/**
	 * @param operation The operation.
	 * @param actualType The type of the underlying element.
	 * @param applicableTypes The types to which the operation is applicable.
	 */
	@SafeVarargs
	public InapplicableOperationError(AstNode operation, Type actualType, Class<? extends Type>... applicableTypes) {
		ArrayList<Class<? extends Type>> appTypes = new ArrayList<>(Arrays.asList(applicableTypes));
		appTypes.sort(Comparator.comparing(Class::getName));
		
		message = format("\n%s can not be applied to type %s.\n" +
						"\nFaulty node in %s:\n%s\n" +
						"\nApplicable types:\n%s\n",
				operation.getClass().getSimpleName(), actualType.getClass().getSimpleName().replace("Type", "").toLowerCase(),
				operation.sourceLocation, highlight(operation.dump()),
				highlight(appTypes.stream().map(type -> type.getSimpleName().replace("Type", "").toLowerCase()).collect(Collectors.joining(", ")))
		);
	}
}
