/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 *
 * All rights reserved.
 *
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.testing;

import mavlc.errors.AstMismatchError;
import mavlc.errors.InternalCompilerError;
import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeBaseVisitor;
import mavlc.syntax.HasDeclaration;
import mavlc.syntax.HasType;
import mavlc.syntax.expression.*;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordTypeDeclaration;
import mavlc.syntax.statement.*;
import mavlc.syntax.type.MatrixTypeSpecifier;
import mavlc.syntax.type.RecordTypeSpecifier;
import mavlc.syntax.type.TypeSpecifier;
import mavlc.syntax.type.VectorTypeSpecifier;

import java.util.List;
import java.util.Objects;

public class AstComparator extends AstNodeBaseVisitor<Void, AstNode> {
	
	@Override protected Void defaultOperation(AstNode node, AstNode refNode) {
		throw new InternalCompilerError("Reached default operation during AST comparison");
	}
	
	private <T extends AstNode> void mismatch(T node, T ref, String s) {
		throw new AstMismatchError(node, ref, s);
	}
	
	public <T extends AstNode> boolean compare(T node, T ref) {
		if(node == ref) return true;
		if(node == null) mismatch(null, ref, "Expected an AST node but got null instead");
		if(!Objects.equals(node.sourceLocation, ref.sourceLocation))
			mismatch(node, ref, "Mismatching source location (got " + node.sourceLocation + ", expected " + ref.sourceLocation + ")");
		if(!Objects.equals(node.getClass(), ref.getClass()))
			mismatch(node, ref, "Expected " + ref.getClass().getSimpleName() + " but got " + node.getClass().getSimpleName());
		node.accept(this, ref);
		if(ref instanceof HasDeclaration && ((HasDeclaration) ref).isDeclarationSet()) {
			if(!((HasDeclaration) node).isDeclarationSet())
				mismatch(node, ref, "No declaration set in generated AST");
			compare(((HasDeclaration) node).getDeclaration(), ((HasDeclaration) ref).getDeclaration());
		}
		if(ref instanceof HasType && ((HasType) ref).isTypeSet()) {
			if(!((HasType) node).isTypeSet())
				mismatch(node, ref, "No type set in generated AST");
			if(!Objects.equals(((HasType) node).getType(), ((HasType) ref).getType()))
				mismatch(node, ref, "Mismatching types");
		}
		return true;
	}
	
	private <T extends AstNode> void compare(T node, T ref, String a, String b, String s) {
		if(!Objects.equals(a, b)) throw new AstMismatchError(node, ref, "Mismatching " + s);
	}
	
	private <T extends AstNode, R extends AstNode> void compare(T node, T ref, List<R> nodes, List<R> refs, String name) {
		if(nodes.size() > (refs != null ? refs.size() : 0))
			mismatch(node, ref, "Too many " + name + " in generated AST");
		if(nodes.size() < (refs != null ? refs.size() : 0))
			mismatch(node, ref, "Too few " + name + " in generated AST");
		for(int i = 0; i < nodes.size(); i++) {
			compare(nodes.get(i), refs.get(i));
		}
	}
	
	@Override public Void visitModule(Module node, AstNode refNode) {
		Module ref = (Module) refNode;
		
		compare(node, ref, node.records, ref.records, "record declarations");
		compare(node, ref, node.functions, ref.functions, "function declarations");
		
		return null;
	}
	
	@Override public Void visitFunction(Function node, AstNode refNode) {
		Function ref = (Function) refNode;
		
		compare(node, ref, node.name, ref.name, "function names");
		compare(node.returnTypeSpecifier, ref.returnTypeSpecifier);
		
		compare(node, ref, node.parameters, ref.parameters, "formal parameters");
		compare(node, ref, node.body, ref.body, "statements");
		
		if(ref.isReturnTypeSet() && !Objects.equals(node.getReturnType(), ref.getReturnType()))
			mismatch(node, ref, "Mismatching return types");
		
		return null;
	}
	
	@Override public Void visitRecordTypeDeclaration(RecordTypeDeclaration node, AstNode refNode) {
		RecordTypeDeclaration ref = (RecordTypeDeclaration) refNode;
		
		compare(node, ref, node.name, ref.name, "record names");
		compare(node, ref, node.elements, ref.elements, "record members");
		
		return null;
	}
	
	@Override public Void visitTypeSpecifier(TypeSpecifier node, AstNode refNode) {
		TypeSpecifier ref = (TypeSpecifier) refNode;
		
		// simple types are singletons, but deserialization creates new instances
		if(node.getClass() != ref.getClass()) mismatch(node, ref, "Mismatching type specifiers");
		
		return null;
	}
	
	@Override public Void visitVectorTypeSpecifier(VectorTypeSpecifier node, AstNode refNode) {
		VectorTypeSpecifier ref = (VectorTypeSpecifier) refNode;
		
		compare(node.elementTypeSpecifier, ref.elementTypeSpecifier);
		compare(node.dimensionExpression, ref.dimensionExpression);
		
		return null;
	}
	
	@Override public Void visitMatrixTypeSpecifier(MatrixTypeSpecifier node, AstNode refNode) {
		MatrixTypeSpecifier ref = (MatrixTypeSpecifier) refNode;
		
		compare(node.elementTypeSpecifier, ref.elementTypeSpecifier);
		compare(node.rowsExpression, ref.rowsExpression);
		compare(node.colsExpression, ref.colsExpression);
		
		return null;
	}
	
	@Override public Void visitRecordTypeSpecifier(RecordTypeSpecifier node, AstNode refNode) {
		RecordTypeSpecifier ref = (RecordTypeSpecifier) refNode;
		
		compare(node, ref, node.recordTypeName, ref.recordTypeName, "types");
		
		return null;
	}
	
	@Override public Void visitVariableAssignment(VariableAssignment node, AstNode refNode) {
		VariableAssignment ref = (VariableAssignment) refNode;
		
		compare(node.identifier, ref.identifier);
		compare(node.value, ref.value);
		
		return null;
	}
	
	@Override public Void visitLeftHandIdentifier(LeftHandIdentifier node, AstNode refNode) {
		LeftHandIdentifier ref = (LeftHandIdentifier) refNode;
		
		compare(node, ref, node.name, ref.name, "identifiers");
		
		return null;
	}
	
	@Override public Void visitMatrixLhsIdentifier(MatrixLhsIdentifier node, AstNode refNode) {
		MatrixLhsIdentifier ref = (MatrixLhsIdentifier) refNode;
		
		visitLeftHandIdentifier(node, ref);
		compare(node.rowIndexExpression, ref.rowIndexExpression);
		compare(node.colIndexExpression, ref.colIndexExpression);
		
		return null;
	}
	
	@Override public Void visitVectorLhsIdentifier(VectorLhsIdentifier node, AstNode refNode) {
		VectorLhsIdentifier ref = (VectorLhsIdentifier) refNode;
		
		visitLeftHandIdentifier(node, ref);
		compare(node.indexExpression, ref.indexExpression);
		
		return null;
	}
	
	@Override public Void visitRecordLhsIdentifier(RecordLhsIdentifier node, AstNode refNode) {
		RecordLhsIdentifier ref = (RecordLhsIdentifier) refNode;
		
		visitLeftHandIdentifier(node, ref);
		compare(node, ref, node.elementName, ref.elementName, "member names");
		
		return null;
	}
	
	@Override public Void visitDeclaration(Declaration node, AstNode refNode) {
		Declaration ref = (Declaration) refNode;
		
		compare(node, ref, node.name, ref.name, "names");
		compare(node.typeSpecifier, ref.typeSpecifier);
		if(node.isVariable() != ref.isVariable())
			mismatch(node, ref, "One declaration is variable, one isn't");
		 if(ref.isLocalBaseOffsetSet() && node.getLocalBaseOffset() != ref.getLocalBaseOffset())
		  mismatch(node, ref, "Mismatching local base offsets");
		
		return null;
	}
	
	@Override public Void visitValueDefinition(ValueDefinition node, AstNode refNode) {
		ValueDefinition ref = (ValueDefinition) refNode;
		
		visitDeclaration(node, ref);
		compare(node.value, ref.value);
		
		return null;
	}
	
	@Override public Void visitForLoop(ForLoop node, AstNode refNode) {
		ForLoop ref = (ForLoop) refNode;
		
		compare(node, ref, node.initVarName, ref.initVarName, "init variable names");
		compare(node.initExpression, ref.initExpression);
		if(ref.isInitVarDeclarationSet() && !Objects.equals(node.getInitVarDeclaration().sourceLocation, ref.getInitVarDeclaration().sourceLocation))
			mismatch(node, ref, "Mismatching init variable declarations");
		compare(node.loopCondition, ref.loopCondition);
		compare(node, ref, node.incrVarName, ref.incrVarName, "increment variable names");
		compare(node.incrExpression, ref.incrExpression);
		if(ref.isIncrVarDeclarationSet() && !Objects.equals(node.getIncrVarDeclaration().sourceLocation, ref.getIncrVarDeclaration().sourceLocation))
			mismatch(node, ref, "Mismatching increment variable declarations");
		compare(node.body, ref.body);
		
		return null;
	}
	
	@Override public Void visitForEachLoop(ForEachLoop node, AstNode refNode) {
		ForEachLoop ref = (ForEachLoop) refNode;
		
		compare(node.iteratorDeclaration, ref.iteratorDeclaration);
		compare(node.structExpression, ref.structExpression);
		compare(node.body, ref.body);
		
		return null;
	}
	
	@Override public Void visitIfStatement(IfStatement node, AstNode refNode) {
		IfStatement ref = (IfStatement) refNode;
		
		compare(node.condition, ref.condition);
		compare(node.thenStatement, ref.thenStatement);
		compare(node.elseStatement, ref.elseStatement);
		
		return null;
	}
	
	@Override public Void visitCallStatement(CallStatement node, AstNode refNode) {
		CallStatement ref = (CallStatement) refNode;
		
		compare(node.callExpression, ref.callExpression);
		
		return null;
	}
	
	@Override public Void visitReturnStatement(ReturnStatement node, AstNode refNode) {
		ReturnStatement ref = (ReturnStatement) refNode;
		
		compare(node.returnValue, ref.returnValue);
		
		return null;
	}
	
	@Override public Void visitCompoundStatement(CompoundStatement node, AstNode refNode) {
		CompoundStatement ref = (CompoundStatement) refNode;
		
		compare(node, ref, node.statements, ref.statements, "statements");
		
		return null;
	}
	
	@Override public Void visitSwitchStatement(SwitchStatement node, AstNode refNode) {
		SwitchStatement ref = (SwitchStatement) refNode;
		
		compare(node.condition, ref.condition);
		compare(node, ref, node.cases, ref.cases, "named cases");
		compare(node, ref, node.defaults, ref.defaults, "default cases");
		
		return null;
	}
	
	@Override public Void visitCase(Case node, AstNode refNode) {
		Case ref = (Case) refNode;
		
		compare(node.conditionExpression, ref.conditionExpression);
		if(ref.isConditionSet() && node.getCondition() != ref.getCondition())
			mismatch(node, ref, "Mismatching conditions");
		compare(node.body, ref.body);
		
		return null;
	}
	
	@Override public Void visitDefault(Default node, AstNode refNode) {
		Default ref = (Default) refNode;
		
		compare(node.body, ref.body);
		
		return null;
	}
	
	@Override public Void visitIdentifierReference(IdentifierReference node, AstNode refNode) {
		IdentifierReference ref = (IdentifierReference) refNode;
		
		compare(node, ref, node.name, ref.name, "names");
		
		return null;
	}
	
	@Override public Void visitSelectExpression(SelectExpression node, AstNode refNode) {
		SelectExpression ref = (SelectExpression) refNode;
		
		compare(node.condition, ref.condition);
		compare(node.trueCase, ref.trueCase);
		compare(node.falseCase, ref.falseCase);
		
		return null;
	}
	
	@Override public Void visitBinaryExpression(BinaryExpression node, AstNode refNode) {
		BinaryExpression ref = (BinaryExpression) refNode;
		
		if(node.getClass() != ref.getClass())
			mismatch(node, ref, "Mismatching operations");
		compare(node.leftOperand, ref.leftOperand);
		compare(node.rightOperand, ref.rightOperand);
		
		return null;
	}
	
	@Override public Void visitCompare(Compare node, AstNode refNode) {
		Compare ref = (Compare) refNode;
		
		visitBinaryExpression(node, ref);
		if(!node.comparator.operator.equals(ref.comparator.operator))
			mismatch(node, ref, "Mismatching comparison operators");
		
		return null;
	}
	
	@Override public Void visitUnaryExpression(UnaryExpression node, AstNode refNode) {
		UnaryExpression ref = (UnaryExpression) refNode;
		
		if(node.getClass() != ref.getClass())
			mismatch(node, ref, "Mismatching operations");
		compare(node.operand, ref.operand);
		
		return null;
	}
	
	@Override public Void visitBoolValue(BoolValue node, AstNode refNode) {
		BoolValue ref = (BoolValue) refNode;
		
		if(node.value != ref.value)
			mismatch(node, ref, "Mismatching literal values");
		
		return null;
	}
	
	@Override public Void visitIntValue(IntValue node, AstNode refNode) {
		IntValue ref = (IntValue) refNode;
		
		if(node.value != ref.value)
			mismatch(node, ref, "Mismatching literal values");
		
		return null;
	}
	
	@Override public Void visitFloatValue(FloatValue node, AstNode refNode) {
		FloatValue ref = (FloatValue) refNode;
		
		if(node.value != ref.value)
			mismatch(node, ref, "Mismatching literal values");
		
		return null;
	}
	
	@Override public Void visitStringValue(StringValue node, AstNode refNode) {
		StringValue ref = (StringValue) refNode;
		
		if(!node.value.equals(ref.value))
			mismatch(node, ref, "Mismatching literal values");
		
		return null;
	}
	
	@Override public Void visitStructureInit(StructureInit node, AstNode refNode) {
		StructureInit ref = (StructureInit) refNode;
		
		compare(node, ref, node.elements, ref.elements, "structure elements");
		
		return null;
	}
	
	@Override public Void visitRecordInit(RecordInit node, AstNode refNode) {
		RecordInit ref = (RecordInit) refNode;
		
		visitStructureInit(node, ref);
		compare(node, ref, node.typeName, ref.typeName, "record names");
		
		return null;
	}
	
	@Override public Void visitCallExpression(CallExpression node, AstNode refNode) {
		CallExpression ref = (CallExpression) refNode;
		
		compare(node, ref, node.functionName, ref.functionName, "callee names");
		compare(node, ref, node.actualParameters, ref.actualParameters, "arguments");
		if(ref.isCalleeDefinitionSet() && !Objects.equals(node.getCalleeDefinition().sourceLocation, ref.getCalleeDefinition().sourceLocation))
			mismatch(node, ref, "Mismatching callee definitions");
		
		return null;
	}
	
	@Override public Void visitElementSelect(ElementSelect node, AstNode refNode) {
		ElementSelect ref = (ElementSelect) refNode;
		
		compare(node.structExpression, ref.structExpression);
		compare(node.indexExpression, ref.indexExpression);
		
		return null;
	}
	
	@Override public Void visitRecordElementSelect(RecordElementSelect node, AstNode refNode) {
		RecordElementSelect ref = (RecordElementSelect) refNode;
		
		compare(node.recordExpression, ref.recordExpression);
		compare(node, ref, node.elementName, ref.elementName, "element names");
		
		return null;
	}
	
	@Override public Void visitSubMatrix(SubMatrix node, AstNode refNode) {
		SubMatrix ref = (SubMatrix) refNode;
		
		compare(node.structExpression, ref.structExpression);
		compare(node.rowStartOffsetExpression, ref.rowStartOffsetExpression);
		compare(node.rowBaseIndexExpression, ref.rowBaseIndexExpression);
		compare(node.rowEndOffsetExpression, ref.rowEndOffsetExpression);
		compare(node.colStartOffsetExpression, ref.colStartOffsetExpression);
		compare(node.colBaseIndexExpression, ref.colBaseIndexExpression);
		compare(node.colEndOffsetExpression, ref.colEndOffsetExpression);
		
		return null;
	}
	
	@Override public Void visitSubVector(SubVector node, AstNode refNode) {
		SubVector ref = (SubVector) refNode;
		
		compare(node.structExpression, ref.structExpression);
		compare(node.startOffsetExpression, ref.startOffsetExpression);
		compare(node.baseIndexExpression, ref.baseIndexExpression);
		compare(node.endOffsetExpression, ref.endOffsetExpression);
		
		return null;
	}
}
