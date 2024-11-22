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

import mavlc.errors.SyntaxError;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.expression.*;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordElementDeclaration;
import mavlc.syntax.record.RecordTypeDeclaration;
import mavlc.syntax.statement.*;
import mavlc.syntax.type.*;

import java.util.*;

import static mavlc.parsing.Token.TokenType.*;
import static mavlc.syntax.expression.Compare.Comparison.*;

/* TODO enter group information
 *
 * EiCB group number: 149
 * Joshua Liam Friedel (279635)
 * Benedikt Schwarz (2373528)
 * Lasse Ramon Reith (2674146)

/**
 * A recursive-descent parser for MAVL.
 */
public final class Parser {

	private final Deque<Token> tokens;
	private Token currentToken;

	/**
	 * @param tokens A token stream that was produced by the {@link Scanner}.
	 */
	public Parser(Deque<Token> tokens) {
		this.tokens = tokens;
		currentToken = tokens.poll();
	}

	/**
	 * Parses the MAVL grammar's start symbol, Module.
	 *
	 * @return A {@link Module} node that is the root of the AST representing the tokenized input program.
	 * @throws SyntaxError to indicate that an unexpected token was encountered.
	 */
	public Module parse() {
		SourceLocation location = currentToken.sourceLocation;

		List<Function> functions = new ArrayList<>();
		List<RecordTypeDeclaration> records = new ArrayList<>();
		while(currentToken.type != EOF) {
			switch(currentToken.type) {
				case FUNCTION:
					functions.add(parseFunction());
					break;
				case RECORD:
					records.add(parseRecordTypeDeclaration());
					break;
				default:
					throw new SyntaxError(currentToken, FUNCTION, RECORD);
			}
		}
		return new Module(location, functions, records);
	}

	private String accept(Token.TokenType type) {
		Token t = currentToken;
		if(t.type != type)
			throw new SyntaxError(t, type);
		acceptIt();
		return t.spelling;
	}

	private void acceptIt() {
		currentToken = tokens.poll();
		if(currentToken == null || currentToken.type == ERROR)
			throw new SyntaxError(currentToken != null ? currentToken : new Token(EOF, null, -1, -1));
	}

	private Function parseFunction() {
		SourceLocation location = currentToken.sourceLocation;

		accept(FUNCTION);
		TypeSpecifier<?> typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);

		List<FormalParameter> parameters = new ArrayList<>();
		List<Statement> body = new ArrayList<>();

		accept(LPAREN);
		if(currentToken.type != RPAREN) {
			parameters.add(parseFormalParameter());
			while(currentToken.type != RPAREN) {
				accept(COMMA);
				parameters.add(parseFormalParameter());
			}
		}
		accept(RPAREN);

		accept(LBRACE);
		while(currentToken.type != RBRACE)
			body.add(parseStatement());
		accept(RBRACE);

		return new Function(location, name, typeSpecifier, parameters, body);
	}

	private FormalParameter parseFormalParameter() {
		SourceLocation location = currentToken.sourceLocation;

		TypeSpecifier<?> typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);

		return new FormalParameter(location, name, typeSpecifier);
	}

	/**
	 * Parses a record type declaration.
	 *
	 * @return A {@link RecordTypeDeclaration} node representing the parsed record type declaration.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private RecordTypeDeclaration parseRecordTypeDeclaration() {
		SourceLocation loc = currentToken.sourceLocation;
		accept(RECORD);
		String recordName = accept(ID);
		accept(LBRACE);
		List<RecordElementDeclaration> elementList = new LinkedList<>();
		RecordElementDeclaration recordElement;
		do { // recordElemDecl+
			recordElement = parseRecordElementDeclaration();
			elementList.add(recordElement);
		} while (currentToken.type == VAR || currentToken.type == VAL);
		accept(RBRACE);
        return new RecordTypeDeclaration(loc, recordName, elementList);
	}

	/**
	 * Parses a record element declaration.
	 *
	 * @return A {@link RecordElementDeclaration} node representing the parsed record element declaration.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private RecordElementDeclaration parseRecordElementDeclaration() {
		SourceLocation loc = currentToken.sourceLocation;
		boolean isVar = false;

		switch (currentToken.type) {
			case VAR:
				isVar = true;
			case VAL:
				acceptIt();
				break;
			default:
				throw new SyntaxError(currentToken, VAR, VAL);
		}

		TypeSpecifier<?> typeSpec = parseTypeSpecifier();
		String elementName = accept(ID);
		accept(SEMICOLON);
        return new RecordElementDeclaration(loc, isVar, typeSpec, elementName);
	}

	private IteratorDeclaration parseIteratorDeclaration() {
		SourceLocation location = currentToken.sourceLocation;

		boolean isVariable;
		switch(currentToken.type) {
			case VAL:
				accept(VAL);
				isVariable = false;
				break;
			case VAR:
				accept(VAR);
				isVariable = true;
				break;
			default:
				throw new SyntaxError(currentToken, VAL, VAR);
		}
		TypeSpecifier<?> typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		return new IteratorDeclaration(location, name, typeSpecifier, isVariable);
	}

	private TypeSpecifier<?> parseTypeSpecifier() {
		SourceLocation location = currentToken.sourceLocation;

		boolean vector = false;
		switch(currentToken.type) {
			case INT:
				acceptIt();
				return new IntTypeSpecifier(location);
			case FLOAT:
				acceptIt();
				return new FloatTypeSpecifier(location);
			case BOOL:
				acceptIt();
				return new BoolTypeSpecifier(location);
			case VOID:
				acceptIt();
				return new VoidTypeSpecifier(location);
			case STRING:
				acceptIt();
				return new StringTypeSpecifier(location);
			case VECTOR:
				accept(VECTOR);
				vector = true;
				break;
			case MATRIX:
				accept(MATRIX);
				break;
			case ID:
				String name = accept(ID);
				return new RecordTypeSpecifier(location, name);
			default:
				throw new SyntaxError(currentToken, INT, FLOAT, BOOL, VOID, STRING, VECTOR, MATRIX, ID);
		}

		accept(LANGLE);
		TypeSpecifier<?> subtype;
		switch(currentToken.type) {
			case INT:
				subtype = new IntTypeSpecifier(currentToken.sourceLocation);
				break;
			case FLOAT:
				subtype = new FloatTypeSpecifier(currentToken.sourceLocation);
				break;
			default:
				throw new SyntaxError(currentToken, INT, FLOAT);
		}
		acceptIt();
		accept(RANGLE);
		accept(LBRACKET);
		Expression x = parseExpr();
		accept(RBRACKET);

		if(vector)
			return new VectorTypeSpecifier(location, subtype, x);

		accept(LBRACKET);
		Expression y = parseExpr();
		accept(RBRACKET);

		return new MatrixTypeSpecifier(location, subtype, x, y);
	}

	private Statement parseStatement() {
		switch(currentToken.type) {
			case VAL:
				return parseValueDef();
			case VAR:
				return parseVarDecl();
			case RETURN:
				return parseReturn();
			case ID:
				return parseAssignOrCall();
			case FOR:
				return parseFor();
			case FOREACH:
				return parseForEach();
			case IF:
				return parseIf();
			case SWITCH:
				return parseSwitch();
			case LBRACE:
				return parseCompound();
			default:
				throw new SyntaxError(currentToken, VAL, VAR, RETURN, ID, FOR, FOREACH, IF, SWITCH, LBRACE);
		}
	}

	/**
	 * Parses a variable declaration.
	 *
	 * @return A {@link VariableDeclaration} node representing the parsed variable declaration.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private ValueDefinition parseValueDef() {
		SourceLocation loc = currentToken.sourceLocation;
		accept(VAL);
		TypeSpecifier<?> parsedType = parseTypeSpecifier();
		String varName = accept(ID);
		accept(ASSIGN);
		Expression valueExpr = parseExpr();
		accept(SEMICOLON);
        return new ValueDefinition(loc, parsedType, varName, valueExpr);
	}

	/**
	 * Parses a variable declaration.
	 *
	 * @return A {@link VariableDeclaration} node representing the parsed variable declaration.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private VariableDeclaration parseVarDecl() {
		SourceLocation srcLoc = currentToken.sourceLocation;
		accept(VAR);
		TypeSpecifier<?> parsedType = parseTypeSpecifier();
		String varIdentifier = accept(ID);
		accept(SEMICOLON);
        return new VariableDeclaration(srcLoc, parsedType, varIdentifier);
	}

	/**
	 * Parses a return statement.
	 *
	 * @return A {@link ReturnStatement} node representing the parsed return statement.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private ReturnStatement parseReturn() {
		SourceLocation loc = currentToken.sourceLocation;
		accept(RETURN);
		Expression returnExpr = parseExpr();
		accept(SEMICOLON);
		return new ReturnStatement(loc, returnExpr);
	}

	/**
	 * Parses an assignment or function call statement.
	 *
	 * @return A {@link Statement} node representing the parsed assignment or function call statement.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private Statement parseAssignOrCall() {
		SourceLocation location = currentToken.sourceLocation;

		String name = accept(ID);

		Statement s;
		if(currentToken.type != LPAREN) {
			s = parseAssign(name, location);
		}
		else {
			s = new CallStatement(location, parseCall(name, location));
		}

		accept(SEMICOLON);

		return s;
	}

	/**
	 * Parses a variable assignment.
	 *
	 * @param name The name of the variable being assigned.
	 * @param location The source location of the assignment.
	 * @return A {@link VariableAssignment} node representing the parsed assignment.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private VariableAssignment parseAssign(String name, SourceLocation location) {
		VariableAssignment assignment;
		LeftHandIdentifier id = new LeftHandIdentifier(location, name);
		if (currentToken.type != ASSIGN) {
			switch (currentToken.type) {
				case LBRACKET:
					acceptIt();
					Expression expr1 = parseExpr();
					accept(RBRACKET);
					if (currentToken.type == LBRACKET) {
						acceptIt();
						Expression expr2 = parseExpr();
						accept(RBRACKET);
						id = new MatrixLhsIdentifier(location, name, expr1, expr2);
						break;
					}
					id = new VectorLhsIdentifier(location, name, expr1);
					break;
				case AT:
					acceptIt();
					String element = accept(ID);
					id = new RecordLhsIdentifier(location, name, element);
					break;
				default:
					throw new SyntaxError(currentToken, LBRACKET, AT, ASSIGN);
			}
		}
		accept(ASSIGN);
		Expression assignedValue = parseExpr();
		assignment = new VariableAssignment(location, id, assignedValue);
		return assignment;
	}

	/**
	 * Parses a function call expression.
	 *
	 * @param name The name of the function being called.
	 * @param location The source location of the call.
	 * @return A {@link CallExpression} node representing the parsed function call.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private CallExpression parseCall(String name, SourceLocation location) {
		// TODO implement (task 1.6)
		accept(LPAREN);
		List<Expression> expressions = new ArrayList<>();
		if(currentToken.type != RPAREN){
			expressions.add(parseExpr());
			while(currentToken.type == COMMA){
				acceptIt();
				expressions.add(parseExpr());
			}
		}
		accept(RPAREN);
		return new CallExpression(location, name, expressions);
	}

	private ForLoop parseFor() {
		SourceLocation location = currentToken.sourceLocation;

		accept(FOR);
		accept(LPAREN);
		String name = accept(ID);
		accept(ASSIGN);
		Expression a = parseExpr();
		accept(SEMICOLON);
		Expression b = parseExpr();
		accept(SEMICOLON);
		String inc = accept(ID);
		accept(ASSIGN);
		Expression c = parseExpr();
		accept(RPAREN);
		return new ForLoop(location, name, a, b, inc, c, parseStatement());
	}

	private ForEachLoop parseForEach() {
		SourceLocation location = currentToken.sourceLocation;

		accept(FOREACH);
		accept(LPAREN);
		IteratorDeclaration param = parseIteratorDeclaration();
		accept(COLON);
		Expression struct = parseExpr();
		accept(RPAREN);
		return new ForEachLoop(location, param, struct, parseStatement());
	}

	/**
	 * Parses an if statement.
	 *
	 * @return An {@link IfStatement} node representing the parsed if statement.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private IfStatement parseIf() {
		// TODO implement method (task 1.3)
		SourceLocation location = currentToken.sourceLocation;
		accept(IF);
		accept(LPAREN);
		Expression expr = parseExpr();
		accept(RPAREN);
		Statement thenStmt = parseStatement();
		if(currentToken.type != ELSE){
			return new IfStatement(location, expr, thenStmt);
		}
		accept(ELSE);
		Statement elseStmt = parseStatement();
		return new IfStatement(location, expr, thenStmt, elseStmt);
	}

	private SwitchStatement parseSwitch() {
		SourceLocation location = currentToken.sourceLocation;
		accept(SWITCH);
		accept(LPAREN);
		Expression condition = parseExpr();
		accept(RPAREN);
		accept(LBRACE);

		List<Case> cases = new ArrayList<>();
		List<Default> defaults = new ArrayList<>();
		while(currentToken.type != RBRACE) {
			if(currentToken.type == CASE)
				cases.add(parseCase());
			else if(currentToken.type == DEFAULT)
				defaults.add(parseDefault());
			else
				throw new SyntaxError(currentToken, CASE, DEFAULT);
		}

		accept(RBRACE);
		return new SwitchStatement(location, condition, cases, defaults);
	}

	private Case parseCase() {
		SourceLocation location = currentToken.sourceLocation;

		accept(CASE);
		Expression caseCond = parseExpr();
		accept(COLON);
		Statement stmt = parseStatement();
		return new Case(location, caseCond, stmt);
	}

	private Default parseDefault() {
		SourceLocation location = currentToken.sourceLocation;

		accept(DEFAULT);
		accept(COLON);
		return new Default(location, parseStatement());
	}

	private CompoundStatement parseCompound() {
		SourceLocation location = currentToken.sourceLocation;

		List<Statement> statements = new ArrayList<>();
		accept(LBRACE);
		while(currentToken.type != RBRACE)
			statements.add(parseStatement());
		accept(RBRACE);

		return new CompoundStatement(location, statements);
	}

	private Expression parseExpr() {
		return parseSelect();
	}

	/**
	 * Parses a select expression (ternary conditional expression).
	 *
	 * @return An {@link Expression} node representing the parsed select expression.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private Expression parseSelect() {
		SourceLocation location = currentToken.sourceLocation;

		Expression cond = parseOr();
		// TODO extend method (task 1.5)
		if(currentToken.type == QMARK){
			acceptIt();
			Expression firstExpr = parseOr();
			accept(COLON);
			Expression secondExpr = parseOr();
			return new SelectExpression(location, cond, firstExpr, secondExpr);
		}
		return cond;
	}


	private Expression parseOr() {
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseAnd();
		while(currentToken.type == OR) {
			acceptIt();
			x = new Or(location, x, parseAnd());
		}
		return x;
	}

	private Expression parseAnd() {
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseNot();
		while(currentToken.type == AND) {
			acceptIt();
			x = new And(location, x, parseNot());
		}
		return x;
	}

	/**
	 * Parses a NOT expression.
	 *
	 * @return An {@link Expression} node representing the parsed NOT expression.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private Expression parseNot() {
		SourceLocation loc = currentToken.sourceLocation;
		if (currentToken.type == NOT) {
			acceptIt();
			return new Not(loc, parseCompare());
		}
		return parseCompare();
	}

	/**
	 * Parses a comparison expression.
	 *
	 * @return An {@link Expression} node representing the parsed comparison expression.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */


	private Expression parseCompare() {
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseAddSub();
		// TODO extend method (task 1.2)
		Expression rightParam;
		Set<Token.TokenType> comparators = Set.of(CMPGE, CMPNE, CMPLE, CMPEQ, LANGLE, RANGLE);
		Map<Token.TokenType, Compare.Comparison> compMap = Map.of(
				CMPGE, GREATER_EQUAL,
				CMPNE, NOT_EQUAL,
				CMPLE, LESS_EQUAL,
				CMPEQ, EQUAL,
				LANGLE, LESS,
				RANGLE, GREATER
		);
		while (comparators.contains(currentToken.type)) {
			Token.TokenType  comp = currentToken.type;
			acceptIt();
			rightParam = parseAddSub();
			x = new Compare(location, x, rightParam, compMap.get(comp));

		}


		return x;
	}

	/**
	 * Parses an addition or subtraction expression.
	 *
	 * @return An {@link Expression} node representing the parsed addition or subtraction expression.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private Expression parseAddSub() {
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseMulDiv();
		// TODO extend method (task 1.2)
		Expression rightParam;
		while (currentToken.type == ADD || currentToken.type == SUB) {
			if (currentToken.type == ADD) {
				acceptIt();
				rightParam = parseMulDiv();
				x = new Addition(location, x, rightParam);
			}
			else{
				acceptIt();
				rightParam = parseMulDiv();
				x = new Subtraction(location, x, rightParam);
			}
		}

		return x;
	}

	/**
	 * Parses a multiplication or division expression.
	 *
	 * @return An {@link Expression} node representing the parsed multiplication or division expression.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private Expression parseMulDiv() {
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseUnaryMinus();
		// TODO extend method (task 1.2)
		Expression rightParam;
		while (currentToken.type == MULT || currentToken.type == DIV) {
			if (currentToken.type == MULT) {
				acceptIt();
				rightParam = parseUnaryMinus();
				x = new Multiplication(location, x, rightParam);
			}
			else{
				acceptIt();
				rightParam = parseUnaryMinus();
				x = new Division(location, x, rightParam);
			}
		}

		return x;
	}


	/**
	 * Parses a unary minus expression.
	 *
	 * @return An {@link Expression} node representing the parsed unary minus expression.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */


	private Expression parseUnaryMinus() {
		SourceLocation loc = currentToken.sourceLocation;
		if (currentToken.type == SUB) {
			acceptIt();
			return new UnaryMinus(loc, parseExponentiation());
		}
		return parseExponentiation();
	}

	/**
	 * Parses an exponentiation expression.
	 *
	 * @return An {@link Expression} node representing the parsed exponentiation expression.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private Expression parseExponentiation() {
		SourceLocation loc = currentToken.sourceLocation;
		Expression leftExpr = parseDotProd();
		if (currentToken.type == EXP) {
			acceptIt();
			return new Exponentiation(loc, leftExpr, parseExponentiation());
		}
		return leftExpr;
	}

	private Expression parseDotProd() {
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseMatrixMul();
		while(currentToken.type == DOTPROD) {
			acceptIt();
			x = new DotProduct(location, x, parseMatrixMul());
		}
		return x;
	}

	private Expression parseMatrixMul() {
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseTranspose();
		while(currentToken.type == MATMULT) {
			acceptIt();
			x = new MatrixMultiplication(location, x, parseTranspose());
		}
		return x;
	}

	/**
	 * Parses a transpose expression.
	 *
	 * @return An {@link Expression} node representing the parsed transpose expression.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private Expression parseTranspose() {
		SourceLocation loc = currentToken.sourceLocation;
		if (currentToken.type == TRANSPOSE) {
			acceptIt();
			return new MatrixTranspose(loc, parseDim());
		}
		return parseDim();
	}

	private Expression parseDim() {
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseSubRange();
		switch(currentToken.type) {
			case ROWS:
				acceptIt();
				return new MatrixRows(location, x);
			case COLS:
				acceptIt();
				return new MatrixCols(location, x);
			case DIM:
				acceptIt();
				return new VectorDimension(location, x);
			default:
				return x;
		}
	}

	private Expression parseSubRange() {
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseElementSelect();

		if(currentToken.type == LBRACE) {
			acceptIt();
			Expression xStartIndex = parseExpr();
			accept(COLON);
			Expression xBaseIndex = parseExpr();
			accept(COLON);
			Expression xEndIndex = parseExpr();
			accept(RBRACE);
			if(currentToken.type != LBRACE)
				return new SubVector(location, x, xBaseIndex, xStartIndex, xEndIndex);

			accept(LBRACE);
			Expression yStartIndex = parseExpr();
			accept(COLON);
			Expression yBaseIndex = parseExpr();
			accept(COLON);
			Expression yEndIndex = parseExpr();
			accept(RBRACE);
			return new SubMatrix(location, x, xBaseIndex, xStartIndex, xEndIndex, yBaseIndex, yStartIndex, yEndIndex);
		}

		return x;
	}

	private Expression parseElementSelect() {
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseRecordElementSelect();

		while(currentToken.type == LBRACKET) {
			acceptIt();
			Expression idx = parseExpr();
			accept(RBRACKET);
			x = new ElementSelect(location, x, idx);
		}

		return x;
	}

	/**
	 * Parses a record element select expression.
	 *
	 * @return An {@link Expression} node representing the parsed record element select expression.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private Expression parseRecordElementSelect() {
		SourceLocation location = currentToken.sourceLocation;

		Expression x = parseAtom();

		// TODO extend method (task 1.7)
		if(currentToken.type == AT){
			acceptIt();
			String name = accept(ID);
			x = new RecordElementSelect(location, x, name);
		}

		return x;
	}

	/**
	 * Parses an atomic expression.
	 *
	 * @return An {@link Expression} node representing the parsed atomic expression.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */



	private Expression parseAtom() {
		SourceLocation location = currentToken.sourceLocation;

		switch(currentToken.type) {
			case INTLIT:
				return new IntValue(location, parseIntLit());
			case FLOATLIT:
				return new FloatValue(location, parseFloatLit());
			case BOOLLIT:
				return new BoolValue(location, parseBoolLit());
			case STRINGLIT:
				return new StringValue(location, accept(STRINGLIT));
			default:
		}

		if(currentToken.type == ID) {
			String name = accept(ID);
			if(currentToken.type != LPAREN) {
				return new IdentifierReference(location, name);
			}
			// TODO extend method (task 1.6)
			return parseCall(name, location);
		}

		if(currentToken.type == LPAREN) {
			acceptIt();
			Expression x = parseExpr();
			accept(RPAREN);
			return x;
		}

		if(currentToken.type == AT){
			acceptIt();
			String element = accept(ID);
			return new RecordInit(location, element, parseInitializerList());
		}


		if(currentToken.type == LBRACKET) {
			return new StructureInit(location, parseInitializerList());
		}

		throw new SyntaxError(currentToken, INTLIT, FLOATLIT, BOOLLIT, STRINGLIT, ID, LPAREN, LBRACKET, AT);
	}

	/**
	 * Parses an initializer list.
	 *
	 * @return A list of {@link Expression} nodes representing the parsed initializer list.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private List<Expression> parseInitializerList() {
		List<Expression> elements = new ArrayList<>();

		accept(LBRACKET);
		elements.add(parseExpr());
		while(currentToken.type == COMMA) {
			accept(COMMA);
			elements.add(parseExpr());
		}
		accept(RBRACKET);

		return elements;
	}

	private int parseIntLit() {
		return Integer.parseInt(accept(INTLIT));
	}

	private float parseFloatLit() {
		return Float.parseFloat(accept(FLOATLIT));
	}

	/**
	 * Parses a boolean literal.
	 *
	 * @return The parsed boolean value.
	 * @throws SyntaxError if an unexpected token is encountered.
	 */

	private boolean parseBoolLit() {
		return Boolean.parseBoolean(accept(BOOLLIT));
	}
}