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
package mavlc.parsing;

import mavlc.syntax.SourceLocation;

/**
 * Token representation.
 */
public final class Token {
	
	/** The token's {@link Token.TokenType type}, e.g., INTLIT or LBRACE. */
	public final TokenType type;
	
	/** The token's spelling, e.g., "foobar_42" for an ID token. */
	public final String spelling;
	
	public final SourceLocation sourceLocation;
	
	/**
	 * @param type The token's {@link Token.TokenType type}, e.g., INTLIT or LBRACE.
	 * @param spelling The token's spelling, e.g., "foobar_42" for an ID token.
	 * @param line The line number in the source file where this token was found.
	 * @param column The column in the line where this token starts.
	 */
	public Token(TokenType type, String spelling, int line, int column) {
		this.type = type;
		this.spelling = spelling;
		this.sourceLocation = new SourceLocation(line, column);
	}
	
	public String toString() {
		return String.format("<%s %s>", type.name(), spelling);
	}
	
	@Override
	public boolean equals(Object obj) {
		return obj instanceof Token && type.equals(((Token) obj).type) && spelling.equals(((Token) obj).spelling);
	}
	
	/**
	 * Enumerates token types used in MAVL.
	 */
	public enum TokenType {
		ID("[a-zA-Z][a-zA-Z0-9_]*"),
		INTLIT("[0-9]+"),
		FLOATLIT("[0-9]+.[0-9]+"),
		BOOLLIT("true|false"),
		STRINGLIT("\".*?\""),
		MATRIX("matrix"),
		VECTOR("vector"),
		INT("int"),
		FLOAT("float"),
		BOOL("bool"),
		VOID("void"),
		STRING("string"),
		VAL("val"),
		VAR("var"),
		FOR("for"),
		FOREACH("foreach"),
		IF("if"),
		ELSE("else"),
		RETURN("return"),
		FUNCTION("function"),
		RECORD("record"),
		AT("@"),
		SWITCH("switch"),
		CASE("case"),
		DEFAULT("default"),
		LPAREN("("),
		RPAREN(")"),
		LBRACE("{"),
		RBRACE("}"),
		LBRACKET("["),
		RBRACKET("]"),
		LANGLE("<"),
		RANGLE(">"),
		COMMA(","),
		SEMICOLON(";"),
		COLON(":"),
		ADD("+"),
		SUB("-"),
		MULT("*"),
		DIV("/"),
		EXP("^"),
		MATMULT("#"),
		DOTPROD(".*"),
		TRANSPOSE("~"),
		QMARK("?"),
		DIM(".dimension"),
		ROWS(".rows"),
		COLS(".cols"),
		CMPLE("<="),
		CMPGE(">="),
		CMPEQ("=="),
		CMPNE("!="),
		ASSIGN("="),
		NOT("!"),
		AND("&"),
		OR("|"),
		EOF("<eof>"),
		ERROR("<error>");
		
		// internal use only
		final String pattern;
		
		TokenType(String pattern) {
			this.pattern = pattern;
		}
	}
}
