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
package mavlc.services.visualization;

import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeBaseVisitor;
import mavlc.syntax.expression.*;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordElementDeclaration;
import mavlc.syntax.record.RecordTypeDeclaration;
import mavlc.syntax.statement.*;
import mavlc.syntax.type.*;

import java.util.List;

public class Dumper extends AstNodeBaseVisitor<Void, Void> {
	
	public static String dump(AstNode node) {
		Dumper visitor = new Dumper();
		visitor.visit(node);
		return visitor.sb.toString();
	}
	
	protected enum AppendType {
		identifier, functionName, type, recordName, recordElementName, keyword, operator, punctuation, string, number, whitespace
	}
	
	protected enum Precedence {
		ternary(0), or(1), and(2), not(3), comparison(4), additive(5), multiplicative(6), unaryMinus(7), exponentiation(8),
		dimension(9), dotProduct(10), matrixMul(11), transpose(12), subRange(13), elementSelect(14), recordElementSelect(15), atom(16);
		
		public final int value;
		
		Precedence(int value) {
			this.value = value;
		}
	}
	
	protected final StringBuilder sb = new StringBuilder();
	
	protected Dumper() { }
	
	protected final String newlineString = "\n";
	protected final String indentString = "\t";
	protected int indentationLevel;
	
	protected void indent() {
		indentationLevel++;
		addNewline();
	}
	
	protected void unindent() {
		indentationLevel--;
		addNewline();
	}
	
	protected void append(Object source, AppendType type) {
		sb.append(source);
	}
	
	protected void addIdentifier(Object source) {
		append(source, AppendType.identifier);
	}
	
	protected void addFunctionName(Object source) {
		append(source, AppendType.functionName);
	}
	
	protected void addType(Object source) {
		append(source, AppendType.type);
	}
	
	protected void addRecordName(Object source) {
		append(source, AppendType.recordName);
	}
	
	protected void addRecordElement(Object source) {
		append(source, AppendType.recordElementName);
	}
	
	protected void addKeyword(Object source) {
		append(source, AppendType.keyword);
	}
	
	protected void addOperator(Object source) {
		append(source, AppendType.operator);
	}
	
	protected void addPunctuation(Object source) {
		append(source, AppendType.punctuation);
	}
	
	protected void addString(Object source) {
		append(source, AppendType.string);
	}
	
	protected void addNumber(Object source) {
		append(source, AppendType.number);
	}
	
	protected void addSpace() {
		append(" ", AppendType.whitespace);
	}
	
	protected void addComma() {
		addPunctuation(",");
		addSpace();
	}
	
	protected void addNewline() {
		append(newlineString, AppendType.whitespace);
		append(new String(new char[indentationLevel]).replace("\0", indentString), AppendType.whitespace);
	}
	
	protected void visitSeparated(List<? extends AstNode> nodes, Runnable separator) {
		if(nodes.isEmpty()) return;
		
		visit(nodes.get(0));
		for(int i = 1; i < nodes.size(); i++) {
			separator.run();
			visit(nodes.get(i));
		}
	}
	
	protected Precedence getPrecedence(Expression expression) {
		if(expression instanceof SelectExpression)
			return Precedence.ternary;
		if(expression instanceof Or)
			return Precedence.or;
		if(expression instanceof And)
			return Precedence.and;
		if(expression instanceof Not)
			return Precedence.not;
		if(expression instanceof Compare)
			return Precedence.comparison;
		if(expression instanceof Addition || expression instanceof Subtraction)
			return Precedence.additive;
		if(expression instanceof Multiplication || expression instanceof Division)
			return Precedence.multiplicative;
		if(expression instanceof UnaryMinus)
			return Precedence.unaryMinus;
		if(expression instanceof Exponentiation)
			return Precedence.exponentiation;
		if(expression instanceof VectorDimension || expression instanceof MatrixRows || expression instanceof MatrixCols)
			return Precedence.dimension;
		if(expression instanceof DotProduct)
			return Precedence.dotProduct;
		if(expression instanceof MatrixMultiplication)
			return Precedence.matrixMul;
		if(expression instanceof MatrixTranspose)
			return Precedence.transpose;
		if(expression instanceof SubVector || expression instanceof SubMatrix)
			return Precedence.subRange;
		if(expression instanceof ElementSelect)
			return Precedence.elementSelect;
		if(expression instanceof RecordElementSelect)
			return Precedence.recordElementSelect;
		return Precedence.atom;
	}
	
	protected void parenthesizeIfNecessary(Expression expression, Precedence precedence) { parenthesizeIfNecessary(expression, precedence, false); }
	
	// strict: whether to parenthesize if precedences are equal
	protected void parenthesizeIfNecessary(Expression expression, Precedence parentPrecedence, boolean strict) {
		Precedence precedence = getPrecedence(expression);
		boolean parenthesize = strict ? precedence.value <= parentPrecedence.value : precedence.value < parentPrecedence.value;
		if(parenthesize) addPunctuation("(");
		visit(expression);
		if(parenthesize) addPunctuation(")");
	}
	
	protected void addExpression(BinaryExpression expression, String operator) { addExpression(expression, operator, false); }
	
	protected void addExpression(BinaryExpression expression, String operator, boolean rightAssociative) {
		Precedence precedence = getPrecedence(expression);
		parenthesizeIfNecessary(expression.leftOperand, precedence, rightAssociative);
		addSpace();
		addOperator(operator);
		addSpace();
		parenthesizeIfNecessary(expression.rightOperand, precedence, !rightAssociative);
	}
	
	
	/* *****************
	 * Visitor methods *
	 ***************** */
	
	@Override public Void visitModule(Module node, Void obj) {
		for(RecordTypeDeclaration record : node.records) {
			visit(record);
			addNewline();
		}
		for(Function function : node.functions) {
			visit(function);
			addNewline();
		}
		return null;
	}
	
	@Override public Void visitFunction(Function node, Void obj) {
		addKeyword("function");
		addSpace();
		visit(node.returnTypeSpecifier);
		addSpace();
		addFunctionName(node.name);
		addPunctuation("(");
		visitSeparated(node.parameters, this::addComma);
		addPunctuation(")");
		addSpace();
		addPunctuation("{");
		{
			indent();
			visitSeparated(node.body, this::addNewline);
			unindent();
		}
		addPunctuation("}");
		addNewline();
		return null;
	}
	
	@Override public Void visitFormalParameter(FormalParameter node, Void obj) {
		visit(node.typeSpecifier);
		addSpace();
		addIdentifier(node.name);
		return null;
	}
	
	@Override public Void visitIteratorDeclaration(IteratorDeclaration node, Void obj) {
		visitDeclaration(node, null);
		return null;
	}
	
	@Override public Void visitRecordTypeDeclaration(RecordTypeDeclaration node, Void obj) {
		addKeyword("record");
		addSpace();
		addRecordName(node.name);
		addSpace();
		addPunctuation("{");
		{
			indent();
			visitSeparated(node.elements, this::addNewline);
			unindent();
		}
		addPunctuation("}");
		addNewline();
		return null;
	}
	
	@Override public Void visitRecordElementDeclaration(RecordElementDeclaration node, Void obj) {
		visitDeclaration(node, null);
		addPunctuation(";");
		return null;
	}
	
	@Override public Void visitDeclaration(Declaration node, Void obj) {
		addKeyword(node.isVariable() ? "var" : "val");
		addSpace();
		visit(node.typeSpecifier);
		addSpace();
		addIdentifier(node.name);
		return null;
	}
	
	@Override public Void visitIntTypeSpecifier(IntTypeSpecifier node, Void obj) {
		addType("int");
		return null;
	}
	
	@Override public Void visitBoolTypeSpecifier(BoolTypeSpecifier node, Void obj) {
		addType("bool");
		return null;
	}
	
	@Override public Void visitFloatTypeSpecifier(FloatTypeSpecifier node, Void obj) {
		addType("float");
		return null;
	}
	
	@Override public Void visitStringTypeSpecifier(StringTypeSpecifier node, Void obj) {
		addType("string");
		return null;
	}
	
	@Override public Void visitVectorTypeSpecifier(VectorTypeSpecifier node, Void obj) {
		addType("vector");
		addPunctuation("<");
		visit(node.elementTypeSpecifier);
		addPunctuation(">");
		addPunctuation("[");
		visit(node.dimensionExpression);
		addPunctuation("]");
		return null;
	}
	
	@Override public Void visitMatrixTypeSpecifier(MatrixTypeSpecifier node, Void obj) {
		addType("matrix");
		addPunctuation("<");
		visit(node.elementTypeSpecifier);
		addPunctuation(">");
		addPunctuation("[");
		visit(node.rowsExpression);
		addPunctuation("]");
		addPunctuation("[");
		visit(node.colsExpression);
		addPunctuation("]");
		return null;
	}
	
	@Override public Void visitRecordTypeSpecifier(RecordTypeSpecifier node, Void obj) {
		addRecordName(node.recordTypeName);
		return null;
	}
	
	@Override public Void visitVoidTypeSpecifier(VoidTypeSpecifier node, Void obj) {
		addType("void");
		return null;
	}
	
	@Override public Void visitValueDefinition(ValueDefinition node, Void obj) {
		visitDeclaration(node, null);
		addSpace();
		addOperator("=");
		addSpace();
		visit(node.value);
		addPunctuation(";");
		return null;
	}
	
	@Override public Void visitVariableDeclaration(VariableDeclaration node, Void obj) {
		visitDeclaration(node, null);
		addPunctuation(";");
		return null;
	}
	
	@Override public Void visitVariableAssignment(VariableAssignment node, Void obj) {
		visit(node.identifier);
		addSpace();
		addOperator("=");
		addSpace();
		visit(node.value);
		addPunctuation(";");
		return null;
	}
	
	@Override public Void visitLeftHandIdentifier(LeftHandIdentifier node, Void obj) {
		addIdentifier(node.name);
		return null;
	}
	
	@Override public Void visitMatrixLhsIdentifier(MatrixLhsIdentifier node, Void obj) {
		addIdentifier(node.name);
		addPunctuation("[");
		visit(node.rowIndexExpression);
		addPunctuation("]");
		addPunctuation("[");
		visit(node.colIndexExpression);
		addPunctuation("]");
		return null;
	}
	
	@Override public Void visitVectorLhsIdentifier(VectorLhsIdentifier node, Void obj) {
		addIdentifier(node.name);
		addPunctuation("[");
		visit(node.indexExpression);
		addPunctuation("]");
		return null;
	}
	
	@Override public Void visitRecordLhsIdentifier(RecordLhsIdentifier node, Void obj) {
		addIdentifier(node.name);
		addRecordElement("@");
		addRecordElement(node.elementName);
		return null;
	}
	
	@Override public Void visitForLoop(ForLoop node, Void obj) {
		addKeyword("for");
		addPunctuation("(");
		addIdentifier(node.initVarName);
		addSpace();
		addOperator("=");
		addSpace();
		visit(node.initExpression);
		addPunctuation(";");
		addSpace();
		visit(node.loopCondition);
		addPunctuation(";");
		addSpace();
		addIdentifier(node.incrVarName);
		addSpace();
		addOperator("=");
		addSpace();
		visit(node.incrExpression);
		addPunctuation(")");
		addSpace();
		visit(node.body);
		return null;
	}
	
	@Override public Void visitForEachLoop(ForEachLoop node, Void obj) {
		addKeyword("foreach");
		addPunctuation("(");
		visit(node.iteratorDeclaration);
		addSpace();
		addPunctuation(":");
		addSpace();
		visit(node.structExpression);
		addPunctuation(")");
		addSpace();
		visit(node.body);
		return null;
	}
	
	@Override public Void visitIfStatement(IfStatement node, Void obj) {
		addKeyword("if");
		addPunctuation("(");
		visit(node.condition);
		addPunctuation(")");
		addSpace();
		visit(node.thenStatement);
		if(node.hasElseStatement()) {
			assert node.elseStatement != null;
			addNewline();
			addKeyword("else");
			addSpace();
			visit(node.elseStatement);
		}
		return null;
	}
	
	@Override public Void visitCallStatement(CallStatement node, Void obj) {
		visit(node.callExpression);
		addPunctuation(";");
		return null;
	}
	
	@Override public Void visitReturnStatement(ReturnStatement node, Void obj) {
		addKeyword("return");
		addSpace();
		visit(node.returnValue);
		addPunctuation(";");
		return null;
	}
	
	@Override public Void visitCompoundStatement(CompoundStatement node, Void obj) {
		addPunctuation("{");
		{
			indent();
			visitSeparated(node.statements, this::addNewline);
			unindent();
		}
		addPunctuation("}");
		return null;
	}
	
	@Override public Void visitSwitchStatement(SwitchStatement node, Void obj) {
		addKeyword("switch");
		addPunctuation("(");
		visit(node.condition);
		addPunctuation(")");
		addSpace();
		addPunctuation("{");
		{
			indent();
			visitSeparated(node.cases, this::addNewline);
			if(!node.defaults.isEmpty()) {
				addNewline();
				visitSeparated(node.defaults, this::addNewline);
			}
			unindent();
		}
		addPunctuation("}");
		return null;
	}
	
	@Override public Void visitCase(Case node, Void obj) {
		addKeyword("case");
		addSpace();
		visit(node.conditionExpression);
		addPunctuation(":");
		addSpace();
		visit(node.body);
		return null;
	}
	
	@Override public Void visitDefault(Default node, Void obj) {
		addKeyword("default");
		addPunctuation(":");
		addSpace();
		visit(node.body);
		return null;
	}
	
	@Override public Void visitIdentifierReference(IdentifierReference node, Void obj) {
		addIdentifier(node.name);
		return null;
	}
	
	@Override public Void visitSelectExpression(SelectExpression node, Void obj) {
		visit(node.condition);
		addSpace();
		addOperator("?");
		addSpace();
		visit(node.trueCase);
		addSpace();
		addOperator(":");
		addSpace();
		visit(node.falseCase);
		return null;
	}
	
	@Override public Void visitMatrixMultiplication(MatrixMultiplication node, Void obj) {
		addExpression(node, "#");
		return null;
	}
	
	@Override public Void visitDotProduct(DotProduct node, Void obj) {
		addExpression(node, ".*");
		return null;
	}
	
	@Override public Void visitExponentiation(Exponentiation node, Void obj) {
		addExpression(node, "^", true);
		return null;
	}
	
	@Override public Void visitMultiplication(Multiplication node, Void obj) {
		addExpression(node, "*");
		return null;
	}
	
	@Override public Void visitDivision(Division node, Void obj) {
		addExpression(node, "/");
		return null;
	}
	
	@Override public Void visitAddition(Addition node, Void obj) {
		addExpression(node, "+");
		return null;
	}
	
	@Override public Void visitSubtraction(Subtraction node, Void obj) {
		addExpression(node, "-");
		return null;
	}
	
	@Override public Void visitCompare(Compare node, Void obj) {
		addExpression(node, node.comparator.operator);
		return null;
	}
	
	@Override public Void visitAnd(And node, Void obj) {
		addExpression(node, "&");
		return null;
	}
	
	@Override public Void visitOr(Or node, Void obj) {
		addExpression(node, "|");
		return null;
	}
	
	@Override public Void visitMatrixTranspose(MatrixTranspose node, Void obj) {
		addOperator("~");
		parenthesizeIfNecessary(node.operand, Precedence.transpose);
		return null;
	}
	
	@Override public Void visitMatrixRows(MatrixRows node, Void obj) {
		parenthesizeIfNecessary(node.operand, Precedence.dimension);
		addPunctuation(".");
		addKeyword("rows");
		return null;
	}
	
	@Override public Void visitMatrixCols(MatrixCols node, Void obj) {
		parenthesizeIfNecessary(node.operand, Precedence.dimension);
		addPunctuation(".");
		addKeyword("cols");
		return null;
	}
	
	@Override public Void visitVectorDimension(VectorDimension node, Void obj) {
		parenthesizeIfNecessary(node.operand, Precedence.dimension);
		addPunctuation(".");
		addKeyword("dimension");
		return null;
	}
	
	@Override public Void visitUnaryMinus(UnaryMinus node, Void obj) {
		addOperator("-");
		parenthesizeIfNecessary(node.operand, Precedence.unaryMinus);
		return null;
	}
	
	@Override public Void visitNot(Not node, Void obj) {
		addOperator("!");
		parenthesizeIfNecessary(node.operand, Precedence.not);
		return null;
	}
	
	@Override public Void visitBoolValue(BoolValue node, Void obj) {
		addKeyword(node.value ? "true" : "false");
		return null;
	}
	
	@Override public Void visitIntValue(IntValue node, Void obj) {
		addNumber(node.value);
		return null;
	}
	
	@Override public Void visitFloatValue(FloatValue node, Void obj) {
		addNumber(node.value);
		return null;
	}
	
	@Override public Void visitStringValue(StringValue node, Void obj) {
		addString(node.getEscapedValue());
		return null;
	}
	
	@Override public Void visitStructureInit(StructureInit node, Void obj) {
		addPunctuation("[");
		visitSeparated(node.elements, this::addComma);
		addPunctuation("]");
		return null;
	}
	
	@Override public Void visitRecordInit(RecordInit node, Void obj) {
		addRecordName("@");
		addRecordName(node.typeName);
		addPunctuation("[");
		visitSeparated(node.elements, this::addComma);
		addPunctuation("]");
		return null;
	}
	
	@Override public Void visitCallExpression(CallExpression node, Void obj) {
		addFunctionName(node.functionName);
		addPunctuation("(");
		visitSeparated(node.actualParameters, this::addComma);
		addPunctuation(")");
		return null;
	}
	
	@Override public Void visitElementSelect(ElementSelect node, Void obj) {
		parenthesizeIfNecessary(node.structExpression, Precedence.elementSelect, false);
		addPunctuation("[");
		visit(node.indexExpression);
		addPunctuation("]");
		return null;
	}
	
	@Override public Void visitRecordElementSelect(RecordElementSelect node, Void obj) {
		parenthesizeIfNecessary(node.recordExpression, Precedence.recordElementSelect, false);
		addRecordElement("@");
		addRecordElement(node.elementName);
		return null;
	}
	
	@Override public Void visitSubMatrix(SubMatrix node, Void obj) {
		parenthesizeIfNecessary(node.structExpression, Precedence.subRange);
		addPunctuation("{");
		visit(node.rowStartOffsetExpression);
		addPunctuation(":");
		visit(node.rowBaseIndexExpression);
		addPunctuation(":");
		visit(node.rowEndOffsetExpression);
		addPunctuation("}");
		addPunctuation("{");
		visit(node.colStartOffsetExpression);
		addPunctuation(":");
		visit(node.colBaseIndexExpression);
		addPunctuation(":");
		visit(node.colEndOffsetExpression);
		addPunctuation("}");
		return null;
	}
	
	@Override public Void visitSubVector(SubVector node, Void obj) {
		parenthesizeIfNecessary(node.structExpression, Precedence.subRange);
		addPunctuation("{");
		visit(node.startOffsetExpression);
		addPunctuation(":");
		visit(node.baseIndexExpression);
		addPunctuation(":");
		visit(node.endOffsetExpression);
		addPunctuation("}");
		return null;
	}
}
