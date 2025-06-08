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
 * AST node visitor interface.
 *
 * @param <RetTy> The return type of the visit methods.
 * @param <ArgTy> The type of the additional argument.
 */
public interface AstNodeVisitor<RetTy, ArgTy> {
	
	// @formatter:off
	
	/* Miscellaneous */
	RetTy visitModule(Module module, ArgTy obj);
	RetTy visitFunction(Function functionNode, ArgTy obj);
	RetTy visitFormalParameter(FormalParameter formalParameter, ArgTy obj);
	RetTy visitIteratorDeclaration(IteratorDeclaration iteratorDeclaration, ArgTy obj);
	RetTy visitRecordTypeDeclaration(RecordTypeDeclaration recordTypeDeclaration, ArgTy obj);
	RetTy visitRecordElementDeclaration(RecordElementDeclaration recordElementDeclaration, ArgTy obj);
	
	/* Types */
	RetTy visitTypeSpecifier(TypeSpecifier<?> typeSpecifier, ArgTy obj);
	RetTy visitIntTypeSpecifier(IntTypeSpecifier intTypeSpecifier, ArgTy obj);
	RetTy visitBoolTypeSpecifier(BoolTypeSpecifier boolTypeSpecifier, ArgTy obj);
	RetTy visitFloatTypeSpecifier(FloatTypeSpecifier floatTypeSpecifier, ArgTy obj);
	RetTy visitStringTypeSpecifier(StringTypeSpecifier stringTypeSpecifier, ArgTy obj);
	RetTy visitVectorTypeSpecifier(VectorTypeSpecifier vectorTypeSpecifier, ArgTy obj);
	RetTy visitMatrixTypeSpecifier(MatrixTypeSpecifier matrixTypeSpecifier, ArgTy obj);
	RetTy visitRecordTypeSpecifier(RecordTypeSpecifier recordTypeSpecifier, ArgTy obj);
	RetTy visitVoidTypeSpecifier(VoidTypeSpecifier voidTypeSpecifier, ArgTy obj);
	
	/* Statements */
	RetTy visitStatement(Statement statement, ArgTy obj);
	RetTy visitDeclaration(Declaration declaration, ArgTy obj);
	RetTy visitValueDefinition(ValueDefinition valueDefinition, ArgTy obj);
	RetTy visitVariableDeclaration(VariableDeclaration variableDeclaration, ArgTy obj);
	RetTy visitVariableAssignment(VariableAssignment variableAssignment, ArgTy obj);
	RetTy visitLeftHandIdentifier(LeftHandIdentifier leftHandIdentifier, ArgTy obj);
	RetTy visitMatrixLhsIdentifier(MatrixLhsIdentifier matrixLhsIdentifier, ArgTy obj);
	RetTy visitVectorLhsIdentifier(VectorLhsIdentifier vectorLhsIdentifier, ArgTy obj);
	RetTy visitRecordLhsIdentifier(RecordLhsIdentifier recordLhsIdentifier, ArgTy obj);
	
	/* Control flow statements*/
	RetTy visitForLoop(ForLoop forLoop, ArgTy obj);
	RetTy visitForEachLoop(ForEachLoop forEachLoop, ArgTy obj);
	RetTy visitIfStatement(IfStatement ifStatement, ArgTy obj);
	RetTy visitCallStatement(CallStatement callStatement, ArgTy obj);
	RetTy visitReturnStatement(ReturnStatement returnStatement, ArgTy obj);
	RetTy visitCompoundStatement(CompoundStatement compoundStatement, ArgTy obj);
	RetTy visitSwitchStatement(SwitchStatement switchCaseStatement, ArgTy obj);
	RetTy visitSwitchSection(SwitchSection switchSection, ArgTy obj);
	RetTy visitCase(Case aCase, ArgTy obj);
	RetTy visitDefault(Default defCase, ArgTy obj);
	
	/* Expressions */
	RetTy visitExpression(Expression expression, ArgTy obj);
	RetTy visitIdentifierReference(IdentifierReference identifierReference, ArgTy obj);
	RetTy visitSelectExpression(SelectExpression expr, ArgTy obj);
	
	/* Binary expressions */
	RetTy visitBinaryExpression(BinaryExpression binaryExpression, ArgTy obj);
	RetTy visitMatrixMultiplication(MatrixMultiplication matrixMultiplication, ArgTy obj);
	RetTy visitDotProduct(DotProduct dotProduct, ArgTy obj);
	RetTy visitExponentiation(Exponentiation exponentiation, ArgTy obj);
	RetTy visitMultiplication(Multiplication multiplication, ArgTy obj);
	RetTy visitDivision(Division division, ArgTy obj);
	RetTy visitAddition(Addition addition, ArgTy obj);
	RetTy visitSubtraction(Subtraction subtraction, ArgTy obj);
	RetTy visitCompare(Compare compare, ArgTy obj);
	RetTy visitAnd(And and, ArgTy obj);
	RetTy visitOr(Or or, ArgTy obj);
	
	/* Unary expressions */
	RetTy visitUnaryExpression(UnaryExpression unaryExpression, ArgTy obj);
	RetTy visitMatrixTranspose(MatrixTranspose matrixTranspose, ArgTy obj);
	RetTy visitMatrixRows(MatrixRows rows, ArgTy obj);
	RetTy visitMatrixCols(MatrixCols cols, ArgTy obj);
	RetTy visitVectorDimension(VectorDimension vectorDimension, ArgTy obj);
	RetTy visitUnaryMinus(UnaryMinus unaryMinus, ArgTy obj);
	RetTy visitNot(Not not, ArgTy obj);
	
	/* Literals */
	RetTy visitBoolValue(BoolValue boolValue, ArgTy obj);
	RetTy visitIntValue(IntValue intValue, ArgTy obj);
	RetTy visitFloatValue(FloatValue floatValue, ArgTy obj);
	RetTy visitStringValue(StringValue stringValue, ArgTy obj);
	RetTy visitStructureInit(StructureInit structureInit, ArgTy obj);
	RetTy visitRecordInit(RecordInit recordInit, ArgTy obj);
	
	/* Other expressions */
	RetTy visitCallExpression(CallExpression callExpression, ArgTy obj);
	RetTy visitElementSelect(ElementSelect elementSelect, ArgTy obj);
	RetTy visitRecordElementSelect(RecordElementSelect recordElementSelect, ArgTy obj);
	RetTy visitSubMatrix(SubMatrix subSelect, ArgTy obj);
	RetTy visitSubVector(SubVector subVector, ArgTy obj);
	// @formatter:on
}
