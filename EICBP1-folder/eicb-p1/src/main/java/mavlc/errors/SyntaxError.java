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

import mavlc.parsing.Token;

import java.util.*;
import java.util.stream.Collectors;

import static java.lang.String.format;
import static mavlc.errors.FormattingHelpers.highlight;

/**
 * Error class to signal a syntax error during parsing.
 */
public class SyntaxError extends CompilationError {
	
	private static final long serialVersionUID = -6933510654953808656L;
	
	/**
	 * @param actualToken The misplaced token.
	 * @param expectedTokens The expected/allowed tokens.
	 */
	public SyntaxError(Token actualToken, Token.TokenType... expectedTokens) {
		List<Token.TokenType> expected = new ArrayList<>(Arrays.asList(expectedTokens));
		
		expected.sort(Comparator.comparing(Enum::name));
		
		switch(expectedTokens.length) {
			case 0:
				message = format("\nSyntax error in %s:\n" +
								"> Unexpected token <%s '%s'>.\n",
						actualToken.sourceLocation, actualToken.type.toString().toLowerCase(), actualToken.spelling
				);
				break;
			case 1:
				message = format("\nSyntax error in %s:\n" +
								"> Got token <%s '%s'> but expected %s.",
						actualToken.sourceLocation, actualToken.type.toString().toLowerCase(), actualToken.spelling, expectedTokens[0].toString().toLowerCase()
				);
				break;
			default:
				message = format("\nSyntax error in %s:\n" +
								"> Got token <%s '%s'>, but expected one of the following:\n%s\n",
						actualToken.sourceLocation, actualToken.type.toString().toLowerCase(), actualToken.spelling,
						highlight(expected.stream().map(token -> token.toString().toLowerCase()).collect(Collectors.joining(", ")))
				);
				break;
		}
	}
}
