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

import mavlc.services.visualization.VisualElement.*;
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
import mavlc.type.RecordType;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@SuppressWarnings("SameParameterValue")
public class Visualizer extends AstNodeBaseVisitor<VisualNode, Void> {
	
	private List<VisualNode> visualNodes;
	private Map<AstNode, VisualNode> astToVisual;
	private Map<AstNode, DeclarationLiteral> declarationLiterals;
	
	/**
	 * Builds a {@link VisualGraph} representing the provided AST node.
	 * The graph can then be exported using a {@link GraphBuilder} like {@link mavlc.services.visualization.dot.DotBuilder}.
	 *
	 * @param node An AST node representing the root of the tree to be generated
	 * @param decorate Whether to include references to the declaration of various identifiers (requires context analysis to be done)
	 * @return The generated {@link VisualGraph}
	 */
	public synchronized VisualGraph buildVisualGraph(AstNode node, boolean decorate) {
		visualNodes = new ArrayList<>();
		astToVisual = new IdentityHashMap<>();
		declarationLiterals = new IdentityHashMap<>();
		VisualNode root = visit(node);
		if(decorate) decorate();
		VisualGraph graph = new VisualGraph(root, visualNodes, astToVisual);
		visualNodes = null;
		astToVisual = null;
		declarationLiterals = null;
		return graph;
	}
	
	private void decorate() {
		for(VisualNode visual : visualNodes) {
			if(visual instanceof ReferenceLiteral) {
				ReferenceLiteral reference = (ReferenceLiteral) visual;
				// only resolve references in decorate() to avoid errors if executed before context analysis
				AstNode targetNode = reference.resolver.get();
				reference.declaration = declarationLiterals.get(targetNode);
			}
		}
		System.out.println();
	}
	
	/* ******************
	 * Helper functions *
	 ****************** */
	
	private void registerNode(VisualNode visual, AstNode ast) {
		visualNodes.add(visual);
		if(ast != null) astToVisual.put(ast, visual);
	}
	
	private VisualNode makeNode(AstNode astNode, String label, NodeElement... children) {
		VisualNode node = new PortNode(label, new ArrayList<>(Arrays.asList(children)));
		registerNode(node, astNode);
		return node;
	}
	
	private VisualNode makeNode(AstNode astNode, String label, List<? extends AstNode> children) {
		List<NodeElement> elements = new ArrayList<>(children.size());
		for(int i = 0; i < children.size(); i++) {
			elements.add(makeChild(Integer.toString(i + 1), children.get(i)));
		}
		VisualNode node = new PortNode(label, elements);
		registerNode(node, astNode);
		return node;
	}
	
	private VisualNode makeSimpleNode(AstNode astNode, String label) {
		return makeSimpleNode(astNode, label, null);
	}
	
	private VisualNode makeSimpleNode(AstNode astNode, String label, VisualNode output) {
		VisualNode node = new SimpleNode(label, output);
		registerNode(node, astNode);
		return node;
	}
	
	private VisualNode makeImmediateLiteral(String content) {
		Literal literal = new Literal(content);
		registerNode(literal, null);
		return literal;
	}
	
	private VisualNode makeImmediateReference(String content, Supplier<AstNode> referenceResolver) {
		ReferenceLiteral literal = new ReferenceLiteral(content, referenceResolver);
		registerNode(literal, null);
		return literal;
	}
	
	private NodeElement makeNamedLiteral(String label, String content) {
		Literal literal = new Literal(content);
		registerNode(literal, null);
		return new Port(label, literal);
	}
	
	private NodeElement makeDeclaration(String label, String content, AstNode owner) {
		DeclarationLiteral literal = new DeclarationLiteral(content);
		registerNode(literal, null);
		declarationLiterals.put(owner, literal);
		return new Port(label, literal);
	}
	
	private NodeElement makeReference(String label, String content, Supplier<AstNode> referenceResolver) {
		ReferenceLiteral literal = new ReferenceLiteral(content, referenceResolver);
		registerNode(literal, null);
		return new Port(label, literal);
	}
	
	private NodeElement makeChild(String label, AstNode node) {
		if(node != null)
			return new Port(label, visit(node));
		return new Port(label, makeImmediateLiteral("<null>"));
	}
	
	private NodeElement makeGroup(String label, NodeElement... children) { return new PortGroup(label, new ArrayList<>(Arrays.asList(children))); }
	
	private NodeElement makeGroup(String label, List<? extends AstNode> children) {
		List<NodeElement> elements = new ArrayList<>(children.size());
		for(int i = 0; i < children.size(); i++) {
			elements.add(makeChild(Integer.toString(i + 1), children.get(i)));
		}
		return new PortGroup(label, elements);
	}
	
	private VisualNode binaryExpression(BinaryExpression node, String label) {
		return makeNode(node, label,
				makeChild("Left", node.leftOperand),
				makeChild("Right", node.rightOperand));
	}
	
	private VisualNode unaryExpression(UnaryExpression node, String label) {
		return makeNode(node, label,
				makeChild("Operand", node.operand));
	}
	
	/* *****************
	 * Visitor methods *
	 ***************** */
	
	//// Modules ////
	
	@Override public VisualNode visitModule(Module node, Void __) {
		return makeNode(node, "Module",
				makeGroup("Records", node.records),
				makeGroup("Functions", node.functions));
	}
	
	@Override public VisualNode visitFunction(Function node, Void __) {
		return makeNode(node, "Function",
				makeChild("ReturnType", node.returnTypeSpecifier),
				makeDeclaration("Name", node.name, node),
				makeGroup("Parameters", node.parameters),
				makeGroup("Statements", node.body));
	}
	
	@Override public VisualNode visitFormalParameter(FormalParameter node, Void __) {
		return makeNode(node, "Param",
				makeChild("Type", node.typeSpecifier),
				makeDeclaration("Name", node.name, node));
	}
	
	@Override public VisualNode visitRecordTypeDeclaration(RecordTypeDeclaration node, Void __) {
		return makeNode(node, "Record",
				makeDeclaration("Name", node.name, node),
				makeGroup("Elements", node.elements));
	}
	
	@Override public VisualNode visitRecordElementDeclaration(RecordElementDeclaration node, Void __) {
		return makeNode(node, node.isVariable() ? "VarElement" : "ValElement",
				makeChild("Type", node.typeSpecifier),
				makeDeclaration("Name", node.name, node));
	}
	
	//// Types ////
	
	@Override public VisualNode visitIntTypeSpecifier(IntTypeSpecifier node, Void __) {
		return makeSimpleNode(node, "IntType");
	}
	
	@Override public VisualNode visitBoolTypeSpecifier(BoolTypeSpecifier node, Void __) {
		return makeSimpleNode(node, "BoolType");
	}
	
	@Override public VisualNode visitFloatTypeSpecifier(FloatTypeSpecifier node, Void __) {
		return makeSimpleNode(node, "FloatType");
	}
	
	@Override public VisualNode visitStringTypeSpecifier(StringTypeSpecifier node, Void __) {
		return makeSimpleNode(node, "StringType");
	}
	
	@Override public VisualNode visitVectorTypeSpecifier(VectorTypeSpecifier node, Void __) {
		return makeNode(node, "VectorType",
				makeChild("ElementType", node.elementTypeSpecifier),
				makeChild("Dimension", node.dimensionExpression));
	}
	
	@Override public VisualNode visitMatrixTypeSpecifier(MatrixTypeSpecifier node, Void __) {
		return makeNode(node, "MatrixType",
				makeChild("ElementType", node.elementTypeSpecifier),
				makeChild("Rows", node.rowsExpression),
				makeChild("Cols", node.colsExpression));
	}
	
	@Override public VisualNode visitRecordTypeSpecifier(RecordTypeSpecifier node, Void __) {
		return makeNode(node, "RecordType",
				makeReference("Name", node.recordTypeName, () -> node.getType().typeDeclaration));
	}
	
	@Override public VisualNode visitVoidTypeSpecifier(VoidTypeSpecifier node, Void __) {
		return makeSimpleNode(node, "VoidType");
	}
	
	//// Statements ////
	
	@Override public VisualNode visitValueDefinition(ValueDefinition node, Void __) {
		return makeNode(node, "ValDef",
				makeChild("Type", node.typeSpecifier),
				makeDeclaration("Name", node.name, node),
				makeChild("Value", node.value));
	}
	
	@Override public VisualNode visitVariableDeclaration(VariableDeclaration node, Void __) {
		return makeNode(node, "VarDecl",
				makeChild("Type", node.typeSpecifier),
				makeDeclaration("Name", node.name, node));
	}
	
	@Override public VisualNode visitVariableAssignment(VariableAssignment node, Void __) {
		return makeNode(node, "Assignment",
				makeChild("Target", node.identifier),
				makeChild("Value", node.value));
	}
	
	@Override public VisualNode visitLeftHandIdentifier(LeftHandIdentifier node, Void __) {
		return makeNode(node, "SimpleLhs",
				makeReference("Name", node.name, node::getDeclaration));
	}
	
	@Override public VisualNode visitMatrixLhsIdentifier(MatrixLhsIdentifier node, Void __) {
		return makeNode(node, "MatrixLhs",
				makeReference("Identifier", node.name, node::getDeclaration),
				makeChild("Row", node.rowIndexExpression),
				makeChild("Column", node.colIndexExpression));
	}
	
	@Override public VisualNode visitVectorLhsIdentifier(VectorLhsIdentifier node, Void __) {
		return makeNode(node, "VectorLhs",
				makeReference("Identifier", node.name, node::getDeclaration),
				makeChild("Index", node.indexExpression));
	}
	
	@Override public VisualNode visitRecordLhsIdentifier(RecordLhsIdentifier node, Void __) {
		return makeNode(node, "RecordLhs",
				makeReference("Identifier", node.name, node::getDeclaration),
				makeReference("Element", node.elementName, () -> ((RecordType) node.getDeclaration().getType()).typeDeclaration.getElement(node.elementName)));
	}
	
	@Override public VisualNode visitForLoop(ForLoop node, Void __) {
		return makeNode(node, "ForLoop",
				makeGroup("Init",
						makeReference("Target", node.initVarName, node::getInitVarDeclaration),
						makeChild("Value", node.initExpression)),
				makeChild("Condition", node.loopCondition),
				makeChild("Body", node.body),
				makeGroup("Inc",
						makeReference("Target", node.incrVarName, node::getIncrVarDeclaration),
						makeChild("Value", node.incrExpression)));
	}
	
	@Override public VisualNode visitForEachLoop(ForEachLoop node, Void __) {
		return makeNode(node, "ForEachLoop",
				makeChild("Iterator", node.iteratorDeclaration),
				makeChild("Struct", node.structExpression),
				makeChild("Body", node.body));
	}
	
	@Override public VisualNode visitIteratorDeclaration(IteratorDeclaration node, Void __) {
		return makeNode(node, node.isVariable() ? "VarIterator" : "ValIterator",
				makeChild("Type", node.typeSpecifier),
				makeDeclaration("Name", node.name, node));
	}
	
	
	@Override public VisualNode visitIfStatement(IfStatement node, Void __) { // @formatter:off
		return node.hasElseStatement()
				? makeNode(node, "If",
					makeChild("Condition", node.condition),
					makeChild("Then", node.thenStatement),
					makeChild("Else", node.elseStatement))
				: makeNode(node, "If",
					makeChild("Condition", node.condition),
					makeChild("Then", node.thenStatement));
	} // @formatter:on
	
	@Override public VisualNode visitCallStatement(CallStatement node, Void __) {
		return makeNode(node, "CallStatement",
				makeChild("CallExpr", node.callExpression));
	}
	
	@Override public VisualNode visitReturnStatement(ReturnStatement node, Void __) {
		return makeNode(node, "Return",
				makeChild("Value", node.returnValue));
	}
	
	@Override public VisualNode visitCompoundStatement(CompoundStatement node, Void __) {
		return makeNode(node, "Compound", node.statements);
	}
	
	@Override public VisualNode visitSwitchStatement(SwitchStatement node, Void __) {
		return makeNode(node, "Switch",
				makeChild("Value", node.condition),
				makeGroup("Cases", Stream.concat(node.cases.stream(), node.defaults.stream()).collect(Collectors.toList())));
	}
	
	@Override public VisualNode visitCase(Case node, Void __) {
		return makeNode(node, "Case",
				makeChild("Condition", node.conditionExpression),
				makeChild("Body", node.body));
	}
	
	@Override public VisualNode visitDefault(Default node, Void __) {
		return makeNode(node, "Default",
				makeChild("Body", node.body));
	}
	
	@Override public VisualNode visitIdentifierReference(IdentifierReference node, Void __) {
		return makeSimpleNode(node, "Identifier",
				makeImmediateReference(node.name, node::getDeclaration));
	}
	
	@Override public VisualNode visitSelectExpression(SelectExpression node, Void __) {
		return makeNode(node, "Select",
				makeChild("Condition", node.condition),
				makeChild("Then", node.trueCase),
				makeChild("Else", node.falseCase));
	}
	
	@Override public VisualNode visitBinaryExpression(BinaryExpression node, Void __) {
		return binaryExpression(node, node.getClass().getSimpleName());
	}
	
	@Override public VisualNode visitMatrixMultiplication(MatrixMultiplication node, Void __) {
		return binaryExpression(node, "MatrixMult");
	}
	
	@Override public VisualNode visitCompare(Compare node, Void __) {
		return makeNode(node, "Compare",
				makeChild("Left", node.leftOperand),
				makeNamedLiteral("Op", node.comparator.operator),
				makeChild("Right", node.rightOperand));
	}
	
	@Override public VisualNode visitUnaryExpression(UnaryExpression node, Void __) {
		return unaryExpression(node, node.getClass().getSimpleName());
	}
	
	@Override public VisualNode visitMatrixTranspose(MatrixTranspose node, Void __) {
		return unaryExpression(node, "Transpose");
	}
	
	@Override public VisualNode visitMatrixRows(MatrixRows node, Void __) {
		return unaryExpression(node, "Rows");
	}
	
	@Override public VisualNode visitMatrixCols(MatrixCols node, Void __) {
		return unaryExpression(node, "Cols");
	}
	
	@Override public VisualNode visitVectorDimension(VectorDimension node, Void __) {
		return unaryExpression(node, "Dimension");
	}
	
	@Override public VisualNode visitUnaryMinus(UnaryMinus node, Void __) {
		return unaryExpression(node, "UnaryMinus");
	}
	
	@Override public VisualNode visitNot(Not node, Void __) {
		return unaryExpression(node, "Not");
	}
	
	//// Literals ////
	
	@Override public VisualNode visitBoolValue(BoolValue node, Void __) {
		return makeSimpleNode(node, "BoolLiteral",
				makeImmediateLiteral(node.value ? "true" : "false"));
	}
	
	@Override public VisualNode visitIntValue(IntValue node, Void __) {
		return makeSimpleNode(node, "IntLiteral",
				makeImmediateLiteral(Integer.toString(node.value)));
	}
	
	@Override public VisualNode visitFloatValue(FloatValue node, Void __) {
		return makeSimpleNode(node, "FloatLiteral",
				makeImmediateLiteral(Float.toString(node.value)));
	}
	
	@Override public VisualNode visitStringValue(StringValue node, Void __) {
		return makeSimpleNode(node, "StringLiteral",
				makeImmediateLiteral(node.getEscapedValue()));
	}
	
	@Override public VisualNode visitStructureInit(StructureInit node, Void __) {
		return makeNode(node, "StructureInit", node.elements);
	}
	
	@Override public VisualNode visitRecordInit(RecordInit node, Void __) {
		return makeNode(node, "RecordInit",
				makeReference("Name", node.typeName, () -> ((RecordType) node.getType()).typeDeclaration),
				makeGroup("Elements", node.elements));
	}
	
	@Override public VisualNode visitCallExpression(CallExpression node, Void __) {
		return makeNode(node, "Call",
				makeReference("Function", node.functionName, node::getCalleeDefinition),
				makeGroup("Arguments", node.actualParameters));
	}
	
	@Override public VisualNode visitElementSelect(ElementSelect node, Void __) {
		return makeNode(node, "ElementSelect",
				makeChild("Struct", node.structExpression),
				makeChild("Index", node.indexExpression));
	}
	
	@Override public VisualNode visitRecordElementSelect(RecordElementSelect node, Void __) {
		return makeNode(node, "RecordElementSelect",
				makeChild("Record", node.recordExpression),
				makeReference("Element", node.elementName, () -> ((RecordType) node.recordExpression.getType()).typeDeclaration.getElement(node.elementName)));
	}
	
	@Override public VisualNode visitSubMatrix(SubMatrix node, Void __) {
		return makeNode(node, "SubMatrix",
				makeChild("Matrix", node.structExpression),
				makeGroup("Row Range",
						makeChild("Start", node.rowStartOffsetExpression),
						makeChild("Base", node.rowBaseIndexExpression),
						makeChild("End", node.rowEndOffsetExpression)),
				makeGroup("Column Range",
						makeChild("Start", node.colStartOffsetExpression),
						makeChild("Base", node.colBaseIndexExpression),
						makeChild("End", node.colEndOffsetExpression)));
	}
	
	@Override public VisualNode visitSubVector(SubVector node, Void __) {
		return makeNode(node, "SubVector",
				makeChild("Vector", node.structExpression),
				makeGroup("Range",
						makeChild("Start", node.startOffsetExpression),
						makeChild("Base", node.baseIndexExpression),
						makeChild("End", node.endOffsetExpression)));
	}
}
