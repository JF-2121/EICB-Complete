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
package mavlc.syntax;

import mavlc.syntax.expression.*;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordElementDeclaration;
import mavlc.syntax.record.RecordTypeDeclaration;
import mavlc.syntax.statement.*;
import mavlc.syntax.type.*;

/**
 * Basic visitor implementation. Each visit method calls the visit method of the
 * corresponding super type.
 *
 * @param <RetTy> Return type used by visitor methods.
 * @param <ArgTy> Argument type used by visitor methods.
 */
public class AstNodeBaseVisitor<RetTy, ArgTy> implements AstNodeVisitor<RetTy, ArgTy> {
	
	// @formatter:off
	public RetTy visit(AstNode node) { return node.accept(this, null); }
	protected RetTy defaultOperation(AstNode node, ArgTy obj) { throw new UnsupportedOperationException(); }
	
	/* Miscellaneous */
	@Override public RetTy visitModule(Module module, ArgTy obj) { return defaultOperation(module, obj); }
	@Override public RetTy visitFunction(Function functionNode, ArgTy obj) { return defaultOperation(functionNode, obj); }
	@Override public RetTy visitRecordTypeDeclaration(RecordTypeDeclaration recordTypeDeclaration, ArgTy obj) { return defaultOperation(recordTypeDeclaration, obj); }
	
	/* Types */
	@Override public RetTy visitTypeSpecifier(TypeSpecifier<?> typeSpecifier, ArgTy obj) { return defaultOperation(typeSpecifier, obj); }
	@Override public RetTy visitIntTypeSpecifier(IntTypeSpecifier intTypeSpecifier, ArgTy obj) { return visitTypeSpecifier(intTypeSpecifier, obj); }
	@Override public RetTy visitBoolTypeSpecifier(BoolTypeSpecifier boolTypeSpecifier, ArgTy obj) { return visitTypeSpecifier(boolTypeSpecifier, obj); }
	@Override public RetTy visitFloatTypeSpecifier(FloatTypeSpecifier floatTypeSpecifier, ArgTy obj) { return visitTypeSpecifier(floatTypeSpecifier, obj); }
	@Override public RetTy visitStringTypeSpecifier(StringTypeSpecifier stringTypeSpecifier, ArgTy obj) { return visitTypeSpecifier(stringTypeSpecifier, obj); }
	@Override public RetTy visitVectorTypeSpecifier(VectorTypeSpecifier vectorTypeSpecifier, ArgTy obj) { return visitTypeSpecifier(vectorTypeSpecifier, obj); }
	@Override public RetTy visitMatrixTypeSpecifier(MatrixTypeSpecifier matrixTypeSpecifier, ArgTy obj) { return visitTypeSpecifier(matrixTypeSpecifier, obj); }
	@Override public RetTy visitRecordTypeSpecifier(RecordTypeSpecifier recordTypeSpecifier, ArgTy obj) { return visitTypeSpecifier(recordTypeSpecifier, obj); }
	@Override public RetTy visitVoidTypeSpecifier(VoidTypeSpecifier voidTypeSpecifier, ArgTy obj) { return visitTypeSpecifier(voidTypeSpecifier, obj); }
	
	/* Statements */
	@Override public RetTy visitStatement(Statement statement, ArgTy obj) { return defaultOperation(statement, obj); }
	@Override public RetTy visitVariableAssignment(VariableAssignment variableAssignment, ArgTy obj) { return visitStatement(variableAssignment, obj); }
	@Override public RetTy visitLeftHandIdentifier(LeftHandIdentifier leftHandIdentifier, ArgTy obj) { return defaultOperation(leftHandIdentifier, obj); }
	@Override public RetTy visitMatrixLhsIdentifier(MatrixLhsIdentifier matrixLhsIdentifier, ArgTy obj) { return visitLeftHandIdentifier(matrixLhsIdentifier, obj); }
	@Override public RetTy visitVectorLhsIdentifier(VectorLhsIdentifier vectorLhsIdentifier, ArgTy obj) { return visitLeftHandIdentifier(vectorLhsIdentifier, obj); }
	@Override public RetTy visitRecordLhsIdentifier(RecordLhsIdentifier recordLhsIdentifier, ArgTy obj) { return visitLeftHandIdentifier(recordLhsIdentifier, obj); }
	
	/* Declarations */
	@Override public RetTy visitDeclaration(Declaration declaration, ArgTy obj) { return visitStatement(declaration, obj); }
	@Override public RetTy visitValueDefinition(ValueDefinition valueDefinition, ArgTy obj) { return visitDeclaration(valueDefinition, obj); }
	@Override public RetTy visitVariableDeclaration(VariableDeclaration variableDeclaration, ArgTy obj) { return visitDeclaration(variableDeclaration, obj); }
	@Override public RetTy visitFormalParameter(FormalParameter formalParameter, ArgTy obj) { return visitDeclaration(formalParameter, obj); }
	@Override public RetTy visitIteratorDeclaration(IteratorDeclaration iteratorDeclaration, ArgTy obj) { return visitDeclaration(iteratorDeclaration, obj); }
	@Override public RetTy visitRecordElementDeclaration(RecordElementDeclaration recordElementDeclaration, ArgTy obj) { return visitDeclaration(recordElementDeclaration, obj); }
	
	/* Control flow statements*/
	@Override public RetTy visitForLoop(ForLoop forLoop, ArgTy obj) { return visitStatement(forLoop, obj); }
	@Override public RetTy visitForEachLoop(ForEachLoop forEachLoop, ArgTy obj) { return visitStatement(forEachLoop, obj); }
	@Override public RetTy visitIfStatement(IfStatement ifStatement, ArgTy obj) { return visitStatement(ifStatement, obj); }
	@Override public RetTy visitCallStatement(CallStatement callStatement, ArgTy obj) { return visitStatement(callStatement, obj); }
	@Override public RetTy visitReturnStatement(ReturnStatement returnStatement, ArgTy obj) { return visitStatement(returnStatement, obj); }
	@Override public RetTy visitCompoundStatement(CompoundStatement compoundStatement, ArgTy obj) { return visitStatement(compoundStatement, obj); }
	@Override public RetTy visitSwitchStatement(SwitchStatement switchCaseStatement, ArgTy obj) { return visitStatement(switchCaseStatement, obj); }
	@Override public RetTy visitSwitchSection(SwitchSection switchSection, ArgTy obj) { return defaultOperation(switchSection, obj); }
	@Override public RetTy visitCase(Case aCase, ArgTy obj) { return visitSwitchSection(aCase, obj); }
	@Override public RetTy visitDefault(Default defCase, ArgTy obj) { return visitSwitchSection(defCase, obj); }
	
	/* Expressions */
	@Override public RetTy visitExpression(Expression expression, ArgTy obj) { return defaultOperation(expression, obj); }
	@Override public RetTy visitIdentifierReference(IdentifierReference identifierReference, ArgTy obj) { return visitExpression(identifierReference, obj); }
	@Override public RetTy visitSelectExpression(SelectExpression expr, ArgTy obj) { return visitExpression(expr, obj); }
	
	/* Binary expressions */
	@Override public RetTy visitBinaryExpression(BinaryExpression binaryExpression, ArgTy obj) { return visitExpression(binaryExpression, obj); }
	@Override public RetTy visitMatrixMultiplication(MatrixMultiplication matrixMultiplication, ArgTy obj) { return visitBinaryExpression(matrixMultiplication, obj); }
	@Override public RetTy visitDotProduct(DotProduct dotProduct, ArgTy obj) { return visitBinaryExpression(dotProduct, obj); }
	@Override public RetTy visitExponentiation(Exponentiation exponentiation, ArgTy obj) { return visitBinaryExpression(exponentiation, obj); }
	@Override public RetTy visitMultiplication(Multiplication multiplication, ArgTy obj) { return visitBinaryExpression(multiplication, obj); }
	@Override public RetTy visitDivision(Division division, ArgTy obj) { return visitBinaryExpression(division, obj); }
	@Override public RetTy visitAddition(Addition addition, ArgTy obj) { return visitBinaryExpression(addition, obj); }
	@Override public RetTy visitSubtraction(Subtraction subtraction, ArgTy obj) { return visitBinaryExpression(subtraction, obj); }
	@Override public RetTy visitCompare(Compare compare, ArgTy obj) { return visitBinaryExpression(compare, obj); }
	@Override public RetTy visitAnd(And and, ArgTy obj) { return visitBinaryExpression(and, obj); }
	@Override public RetTy visitOr(Or or, ArgTy obj) { return visitBinaryExpression(or, obj); }
	
	/* Unary expressions */
	@Override public RetTy visitUnaryExpression(UnaryExpression unaryExpression, ArgTy obj) { return visitExpression(unaryExpression, obj); }
	@Override public RetTy visitMatrixTranspose(MatrixTranspose matrixTranspose, ArgTy obj) { return visitUnaryExpression(matrixTranspose, obj); }
	@Override public RetTy visitMatrixRows(MatrixRows rows, ArgTy obj) { return visitUnaryExpression(rows, obj); }
	@Override public RetTy visitMatrixCols(MatrixCols cols, ArgTy obj) { return visitUnaryExpression(cols, obj); }
	@Override public RetTy visitVectorDimension(VectorDimension vectorDimension, ArgTy obj) { return visitUnaryExpression(vectorDimension, obj); }
	@Override public RetTy visitUnaryMinus(UnaryMinus unaryMinus, ArgTy obj) { return visitUnaryExpression(unaryMinus, obj); }
	@Override public RetTy visitNot(Not not, ArgTy obj) { return visitUnaryExpression(not, obj); }
	
	/* Literals */
	@Override public RetTy visitBoolValue(BoolValue boolValue, ArgTy obj) { return visitExpression(boolValue, obj); }
	@Override public RetTy visitIntValue(IntValue intValue, ArgTy obj) { return visitExpression(intValue, obj); }
	@Override public RetTy visitFloatValue(FloatValue floatValue, ArgTy obj) { return visitExpression(floatValue, obj); }
	@Override public RetTy visitStringValue(StringValue stringValue, ArgTy obj) { return visitExpression(stringValue, obj); }
	@Override public RetTy visitStructureInit(StructureInit structureInit, ArgTy obj) { return visitExpression(structureInit, obj); }
	@Override public RetTy visitRecordInit(RecordInit recordInit, ArgTy obj) { return visitStructureInit(recordInit, obj); }
	
	/* Other expressions */
	@Override public RetTy visitCallExpression(CallExpression callExpression, ArgTy obj) { return visitExpression(callExpression, obj); }
	@Override public RetTy visitElementSelect(ElementSelect elementSelect, ArgTy obj) { return visitExpression(elementSelect, obj); }
	@Override public RetTy visitRecordElementSelect(RecordElementSelect recordElementSelect, ArgTy obj) { return visitExpression(recordElementSelect, obj); }
	@Override public RetTy visitSubMatrix(SubMatrix subMatrix, ArgTy obj) { return visitExpression(subMatrix, obj); }
	@Override public RetTy visitSubVector(SubVector subVector, ArgTy obj) { return visitExpression(subVector, obj); }
	// @formatter:on
}
