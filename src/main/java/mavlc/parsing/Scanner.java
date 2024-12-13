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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Deque;
import java.util.List;

/**
 * A simple scanner for MAVL.
 */
public final class Scanner {

	private int currentLine = 1, currentColumn = 0;
	private int lastLine = 1, lastColumn = 0;
	private int currentChar;
	private StringBuilder currentSpelling;
	private final FileInputStream fileInputStream;

	private static final List<Token.TokenType> keywords = Arrays.asList(
			Token.TokenType.INT, Token.TokenType.FLOAT, Token.TokenType.BOOL, Token.TokenType.VOID, Token.TokenType.STRING, Token.TokenType.MATRIX, Token.TokenType.VECTOR,
			Token.TokenType.VAL, Token.TokenType.VAR, Token.TokenType.FOR, Token.TokenType.IF, Token.TokenType.ELSE, Token.TokenType.RETURN, Token.TokenType.FUNCTION, Token.TokenType.SWITCH, Token.TokenType.CASE, Token.TokenType.DEFAULT, Token.TokenType.FOREACH, Token.TokenType.RECORD);

	/**
	 * @param file The input program to tokenize.
	 * @throws IOException in case an error occurs while accessing the given file.
	 */
	public Scanner(File file) throws IOException {
		fileInputStream = new FileInputStream(file);
		currentChar = fileInputStream.read();
	}

	/**
	 * Scans the input program.
	 *
	 * @return A queue containing the tokenized representation of the input program.
	 */
	public Deque<Token> scan() {
		ArrayDeque<Token> result = new ArrayDeque<>();
		while(currentChar != -1) {

			/* Skip all whitespaces immediately */
			while(currentChar == ' ' || currentChar == '\n' || currentChar == '\r' || currentChar == '\t')
				skipIt();

			currentSpelling = new StringBuilder(16);
			lastLine = this.currentLine;
			lastColumn = this.currentColumn;

			/* Deal with line and block comments */
			if(currentChar == '/') {
				takeIt();

				// Line comments
				if(currentChar == '/') {
					while(currentChar != '\n' && currentChar != -1)
						skipIt();
					continue;
				}

				// Block comments
				if(currentChar == '*') {
					skipIt();
					while(true) {
						if(currentChar == '*') {
							skipIt();
							if(currentChar == '/') {
								skipIt();
								break;
							}
						} else {
							skipIt();
						}
					}
					continue;
				}

				// is actually operator
				result.add(new Token(Token.TokenType.DIV, currentSpelling.toString(), lastLine, lastColumn));
				continue;
			}

			if(currentChar != -1) {
				Token.TokenType type = scanToken();
				result.add(new Token(type, currentSpelling.toString(), lastLine, lastColumn));
			}
		}

		result.add(new Token(Token.TokenType.EOF, Token.TokenType.EOF.pattern, currentLine, currentColumn));
		return result;
	}

	private void takeIt() {
		currentSpelling.append((char) currentChar);
		skipIt();
	}

	private void skipIt() {
		if((char) currentChar == '\n') {
			currentLine++;
			currentColumn = 0;
		} else if((char) currentChar == '\t') {
			final int tabWidth = 4;
			currentColumn = (currentColumn/tabWidth+1)*tabWidth;
		} else {
			currentColumn++;
		}
		try {
			int old = currentChar;
			currentChar = fileInputStream.read();
			if(old == -1 && currentChar == -1) {
				throw new RuntimeException(
						String.format("Reached EOF while scanning, Token started at line %d, column %d",
								lastLine, lastColumn));
			}
		} catch(IOException e) {
			e.printStackTrace();
		}
	}

	private Token.TokenType scanKeywordOrIdentifier() {
		takeIt();
		while(isLetter(currentChar) || isDigit(currentChar) || currentChar == '_')
			takeIt();

		String spelling = currentSpelling.toString();

		if("true".equals(spelling) || "false".equals(spelling))
			return Token.TokenType.BOOLLIT;

		for(Token.TokenType keyword : keywords)
			if(keyword.pattern.contentEquals(currentSpelling))
				return keyword;

		return Token.TokenType.ID;
	}

	private Token.TokenType scanNumber() {
		takeIt();
		while(isDigit(currentChar) || currentChar == '.')
			takeIt();
		return currentSpelling.toString().contains(".") ? Token.TokenType.FLOATLIT : Token.TokenType.INTLIT;
	}

	private Token.TokenType scanString() {
		skipIt();
		while(currentChar != '"') {
			if(currentChar != '\\') {
				takeIt();
				continue;
			}
			skipIt(); // skip backslash
			switch(currentChar) {
				case '"':
				case '\\':
					takeIt();
					break;

				case 'n':
					currentSpelling.append('\n');
					skipIt();
					break;

				case 'r':
					currentSpelling.append('\r');
					skipIt();
					break;

				case 't':
					currentSpelling.append('\t');
					skipIt();
					break;

				default:
					System.err.println("Invalid escape sequence: \\" + currentChar);
					takeIt();
					break;
			}
		}
		skipIt();
		return Token.TokenType.STRINGLIT;
	}

	private Token.TokenType scanDot() {
		takeIt();

		if(currentChar == '*') {
			takeIt();
			return Token.TokenType.DOTPROD;
		}

		while(isLetter(currentChar))
			takeIt();

		if(currentSpelling.toString().equals(Token.TokenType.DIM.pattern))
			return Token.TokenType.DIM;
		if(currentSpelling.toString().equals(Token.TokenType.ROWS.pattern))
			return Token.TokenType.ROWS;
		if(currentSpelling.toString().equals(Token.TokenType.COLS.pattern))
			return Token.TokenType.COLS;

		return Token.TokenType.ERROR;
	}

	private Token.TokenType scanToken() {
		if(isLetter(currentChar))
			return scanKeywordOrIdentifier();

		if(isDigit(currentChar))
			return scanNumber();

		if(currentChar == '\"')
			return scanString();

		if(currentChar == '.')
			return scanDot();

		int cc = currentChar;
		takeIt();
		switch(cc) {
			case ';':
				return Token.TokenType.SEMICOLON;
			case ',':
				return Token.TokenType.COMMA;
			case '(':
				return Token.TokenType.LPAREN;
			case ')':
				return Token.TokenType.RPAREN;
			case '[':
				return Token.TokenType.LBRACKET;
			case ']':
				return Token.TokenType.RBRACKET;
			case '{':
				return Token.TokenType.LBRACE;
			case '}':
				return Token.TokenType.RBRACE;
			case '*':
				return Token.TokenType.MULT;
			case '/':
				return Token.TokenType.DIV;
			case '+':
				return Token.TokenType.ADD;
			case '-':
				return Token.TokenType.SUB;
			case ':':
				return Token.TokenType.COLON;
			case '#':
				return Token.TokenType.MATMULT;
			case '~':
				return Token.TokenType.TRANSPOSE;
			case '?':
				return Token.TokenType.QMARK;
			case '&':
				return Token.TokenType.AND;
			case '|':
				return Token.TokenType.OR;
			case '@':
				return Token.TokenType.AT;
			case '^':
				return Token.TokenType.EXP;
			case '<':
				if(currentChar == '=') {
					takeIt();
					return Token.TokenType.CMPLE;
				}
				return Token.TokenType.LANGLE;
			case '>':
				if(currentChar == '=') {
					takeIt();
					return Token.TokenType.CMPGE;
				}
				return Token.TokenType.RANGLE;
			case '=':
				if(currentChar == '=') {
					takeIt();
					return Token.TokenType.CMPEQ;
				}
				return Token.TokenType.ASSIGN;
			case '!':
				if(currentChar == '=') {
					takeIt();
					return Token.TokenType.CMPNE;
				}
				return Token.TokenType.NOT;
		}
		return Token.TokenType.ERROR;
	}

	private boolean isLetter(int c) {
		return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
	}

	private boolean isDigit(int c) {
		return (c >= '0' && c <= '9');
	}
}
