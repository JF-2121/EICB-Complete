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
package mavlc.codegen.tam;

import mavlc.errors.InternalCompilerError;
import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeBaseVisitor;
import mavlc.syntax.expression.*;
import mavlc.syntax.expression.Compare.Comparison;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordElementDeclaration;
import mavlc.syntax.statement.*;
import mavlc.type.*;
import mtam.Instruction;
import mtam.Primitive;
import mtam.Register;
import mtam.interpreter.Value;

import java.util.ArrayList;
import java.util.List;

/* TODO enter group information
 *
 * EiCB group number: 149
 * Joshua Liam Friedel (279635)
 * Benedikt Schwarz (2373528)
 * Lasse Ramon Reith (2674146)
 */

public class CodeGenerator extends AstNodeBaseVisitor<Instruction, Void> {

	protected final TamAssembler assembler;

	public CodeGenerator(TamAssembler asm) {
		assembler = asm;
	}

	@Override
	protected Instruction defaultOperation(AstNode node, Void __) {
		throw new UnsupportedOperationException("Code generation for this element is not implemented!");
	}

	/**
	 * A wrapper around the <code>AstNode.accept</code> method which also manages the context stack required for generating certain debug symbols.
	 *
	 * @param node The node to visit (node.accept(this) will be called).
	 * @return The return value of the accept() call.
	 */
	@Override
	public Instruction visit(AstNode node) {
		assembler.pushContext(node);
		Instruction result = node.accept(this);
		assembler.popContext();
		return result;
	}

	@Override
	public Instruction visitModule(Module module, Void __) {
		module.functions.forEach(this::visit);
		return null;
	}

	@Override
	public Instruction visitFunction(Function functionNode, Void __) {
		assembler.addNewFunction(functionNode);

		// set local base offset of all parameters
		int argOffset = 0;
		for(int i = functionNode.parameters.size() - 1; i >= 0; i--) {
			FormalParameter param = functionNode.parameters.get(i);
			argOffset -= param.getType().wordSize;
			param.setLocalBaseOffset(argOffset);
		}

		// generate code for statements
		functionNode.body.forEach(this::visit);

		// emit return here, since the arg offset is required
		assembler.emitReturn(functionNode.getReturnType().wordSize, -argOffset);

		return null;
	}

	@Override
	public Instruction visitValueDefinition(ValueDefinition valueDefinition, Void __) {
		visit(valueDefinition.value);
		assembler.addDeclaredEntity(valueDefinition);
		return null;
	}

	@Override
	public Instruction visitVariableDeclaration(VariableDeclaration variableDeclaration, Void __) {
		Type mavlType = variableDeclaration.getType();

		// Zero-initialize stack space for this variable
		assembler.
				emitPush(mavlType.wordSize)
				.addName(variableDeclaration.name)
				// The MTAM tracks types of stack entries for verification purposes, so set the right type for this
				// variable
				.addType(Value.Type.fromMavl(mavlType));
		assembler.addDeclaredEntity(variableDeclaration);
		return null;
	}

	@Override
	public Instruction visitVariableAssignment(VariableAssignment variableAssignment, Void __) {
		// TODO implement (task 3.3)
		// This method is responsible for handling a variable assignment operation.
		// It needs to first evaluate the expression on the right-hand side of the assignment,
		// then load the address of the left-hand identifier, and finally store the value into
		// that memory location.

		// Evaluate the value being assigned (right-hand side)
		visit(variableAssignment.value);

		// Load the address of the variable's local base offset from its declaration
		visit(variableAssignment.identifier);

		// Store the value to the stack at the appropriate offset for the variable
		assembler.storeToStackAddress(1);

		return null;
	}

	@Override
	public Instruction visitLeftHandIdentifier(LeftHandIdentifier leftHandIdentifier, Void __) {
		// TODO implement (task 3.3)
		// This method is responsible for loading the address of a variable's local base offset.
		// It needs to fetch this information from the variable's declaration and set up an instruction
		// to load the address into a specific register.

		// Load the address of the left-hand side identifier using its local base offset
		assembler.loadAddress(Register.LB, leftHandIdentifier.getDeclaration().getLocalBaseOffset());

		return null;
	}

	@Override
	public Instruction visitMatrixLhsIdentifier(MatrixLhsIdentifier matrixLhsIdentifier, Void __) {
		// TODO implement (task 3.3)
		// This method is responsible for loading the address of a matrix's element.
		// It needs to evaluate the row and column indices, perform bounds checking,
		// calculate the offset from the base address of the matrix, and load the address into a register.

		int localOffset = matrixLhsIdentifier.getDeclaration().getLocalBaseOffset();
		MatrixType matrixType = (MatrixType) matrixLhsIdentifier.getDeclaration().getType();
		int NumOfRows = matrixType.rows;
		int NumOfCols = matrixType.cols;

		// Evaluate the column index expression
		visit(matrixLhsIdentifier.colIndexExpression);

		// Perform bounds checking on the column index to ensure it is within valid range
		assembler.emitBoundsCheck(0, NumOfCols);

		// Evaluate the row index expression
		visit(matrixLhsIdentifier.rowIndexExpression);

		// Perform bounds checking on the row index to ensure it is within valid range
		assembler.emitBoundsCheck(0, NumOfRows);

		// Calculate the offset from the base address of the matrix using the row and column indices
		assembler.loadIntegerValue(NumOfCols);
		assembler.emitIntegerMultiplication();
		assembler.emitIntegerAddition();

		// Load the local base address of the matrix
		assembler.loadAddress(Register.LB, localOffset);

		// Add the calculated offset to the base address to get the actual element's address
		assembler.emitIntegerAddition();

		return null;
	}

	@Override
	public Instruction visitVectorLhsIdentifier(VectorLhsIdentifier vectorLhsIdentifier, Void __) {
		// TODO implement (task 3.3)
		// This method is responsible for loading the address of a vector's element.
		// It needs to evaluate the index expression, perform bounds checking,
		// calculate the offset from the base address of the vector, and load the address into a register.

		int localOffset = vectorLhsIdentifier.getDeclaration().getLocalBaseOffset();
		VectorType vectorType  = (VectorType) vectorLhsIdentifier.getDeclaration().getType();
		int vectordim = vectorType.dimension;

		// Evaluate the index expression
		visit(vectorLhsIdentifier.indexExpression);

		// Perform bounds checking on the index to ensure it is within valid range
		assembler.emitBoundsCheck(0, vectordim);

		// Load the local base address of the vector
		assembler.loadAddress(Register.LB, localOffset);

		// Add the calculated offset to the base address to get the actual element's address
		assembler.emitIntegerAddition();

		return null;
	}

	@Override
	public Instruction visitRecordLhsIdentifier(RecordLhsIdentifier recordLhsIdentifier, Void __) {
		// TODO implement (task 3.3)
		// This method is responsible for loading the address of a record's field.
		// It needs to evaluate the element name, calculate its offset from the base address of the record,
		// and load the address into a register.

		int localOffset = recordLhsIdentifier.getDeclaration().getLocalBaseOffset();
		RecordType recordType = (RecordType) recordLhsIdentifier.getDeclaration().getType();
		int elementOffset = recordType.typeDeclaration.getElementOffset(recordLhsIdentifier.elementName);

		// Load the local base address of the record
		assembler.loadAddress(Register.LB, localOffset);

		// Add the calculated offset to the base address to get the actual field's address
		assembler.loadIntegerValue(elementOffset);
		assembler.emitIntegerAddition();

		return null;
	}

	@Override
	public Instruction visitForLoop(ForLoop forLoop, Void __) {
		// TODO implement (task 3.5)
		// This method is responsible for generating code to handle a for loop.
		// It needs to initialize the loop variable, set up the condition check,
		// and generate the necessary instructions for iterating through the loop.

		int initOffset = forLoop.getInitVarDeclaration().getLocalBaseOffset();
		int incrOffset = forLoop.getIncrVarDeclaration().getLocalBaseOffset();

		// Evaluate and store the initial value of the loop variable
		visit(forLoop.initExpression);
		assembler.storeLocalValue(forLoop.initExpression.getType().wordSize, initOffset);

		// Create a jump instruction to skip the body if the condition is false
		Instruction jumpToCheck = assembler.emitJump(-1);

		int loopStart = assembler.getNextInstructionAddress();
		int savedOffset = assembler.getNextOffset();

		// Visit and generate code for the loop body
		visit(forLoop.body);

		// Reset the next offset to before the jump was created so we can store the increment expression later
		assembler.resetNextOffset(savedOffset);

		// Evaluate and store the incremented value of the loop variable
		visit(forLoop.incrExpression);
		int wordSize = forLoop.incrExpression.getType().wordSize;
		assembler.storeLocalValue(wordSize, incrOffset);

		// Load the condition into a register to check if it's true or false
		int checkCondition = assembler.getNextInstructionAddress();
		assembler.backPatchJump(jumpToCheck, checkCondition);

		// Visit and evaluate the loop condition expression
		visit(forLoop.loopCondition);

		// Emit a conditional jump instruction to jump back to the start of the loop if the condition is true
		assembler.emitConditionalJump(true, loopStart);

		return null;
	}


	@Override
	public Instruction visitForEachLoop(ForEachLoop forEachLoop, Void __) {
		// save current stack size reserved for locals
		int localSize = assembler.getNextOffset();

		Expression struct = forEachLoop.structExpression;
		IteratorDeclaration iterator = forEachLoop.iteratorDeclaration;
		int elementCount = struct.getType().wordSize;

		// get base address of the struct and iterator lb offset
		int structBase;
		boolean popStruct = false;
		if(struct instanceof IdentifierReference) {
			// use base address of the identifier
			structBase = ((IdentifierReference) struct).getDeclaration().getLocalBaseOffset();
		} else {
			// evaluate the struct, base address is the current stack top
			popStruct = true;
			structBase = assembler.getNextOffset();
			visit(struct);
			// reserve stack space for struct
			assembler.setNextOffset(assembler.getNextOffset() + elementCount);
		}


		// load initial value for i
		assembler.loadIntegerValue(0);
		assembler.setNextOffset(assembler.getNextOffset() + 1);
		// reserve space for iterator
		iterator.setLocalBaseOffset(assembler.getNextOffset());
		assembler.setNextOffset(assembler.getNextOffset() + 1);

		// loop condition (i < struct.wordSize)
		int loopCondition = assembler.getNextInstructionAddress();
		// ..., i
		assembler.loadValue(Register.ST, 1, -1);
		// ..., i, i
		assembler.loadIntegerValue(elementCount);
		// ..., i, i, count
		assembler.emitIntegerComparison(Comparison.LESS);
		// ..., i, bool
		Instruction jumpToLoopEnd = assembler.emitConditionalJump(false, -1);
		// ..., i

		// loop body
		{
			// populate iterator (cur = struct[i])
			// ..., i
			assembler.loadValue(Register.ST, 1, -1);
			// ..., i, i
			assembler.loadAddress(Register.LB, structBase);
			// ..., i, i, &struct
			assembler.emitIntegerAddition();
			// ..., i, &struct[i]
			assembler.loadFromStackAddress(1);
			// ..., i, cur

			// execute body
			int nextOffset = assembler.getNextOffset();
			visit(forEachLoop.body);
			assembler.resetNextOffset(nextOffset);

			// save value and proceed to next iteration
			if(iterator.isVariable()) {
				// ..., i, cur
				assembler.loadValue(Register.ST, 1, -2);
				// ..., i, cur, i
				assembler.loadAddress(Register.LB, structBase);
				// ..., i, cur, i, &struct
				assembler.emitIntegerAddition();
				// ..., i, cur, &struct[i]
				assembler.storeToStackAddress(1);
				// ..., i
			} else {
				// ..., i, cur
				assembler.emitPop(0, 1);
				// ..., i
			}
			// ..., i
			assembler.emitIncrement();
			// ..., i+1
		}
		assembler.emitJump(loopCondition);

		int loopEnd = assembler.getNextInstructionAddress();
		assembler.backPatchJump(jumpToLoopEnd, loopEnd);

		// pop auxiliary values
		if(popStruct)
			assembler.emitPop(0, elementCount + 1);
		else
			assembler.emitPop(0, 1);

		// reset local stack size
		assembler.setNextOffset(localSize);
		return null;
	}

	@Override
	public Instruction visitIfStatement(IfStatement ifStatement, Void __) {
		int nextOffset = assembler.getNextOffset();
		boolean emitElse = ifStatement.hasElseStatement();

		// evaluate condition
		visit(ifStatement.condition);
		// if the condition is false, skip the 'then' branch
		Instruction jumpOverThenBranch = assembler.emitConditionalJump(false, -1);

		// emit then branch
		visit(ifStatement.thenStatement);
		assembler.resetNextOffset(nextOffset);

		// if the statement has an else branch, emit a jump to skip it
		Instruction jumpOverElseBranch = null;
		if(emitElse) jumpOverElseBranch = assembler.emitJump(-1);

		// backpatch jump over 'then' branch
		int endOfThenBranch = assembler.getNextInstructionAddress();
		assembler.backPatchJump(jumpOverThenBranch, endOfThenBranch);

		if(emitElse) {
			assert ifStatement.elseStatement != null;

			// emit else branch
			visit(ifStatement.elseStatement);
			assembler.resetNextOffset(nextOffset);

			// backpatch jump over 'else' branch
			int endOfElseBranch = assembler.getNextInstructionAddress();
			assembler.backPatchJump(jumpOverElseBranch, endOfElseBranch);
		}
		return null;
	}

	@Override
	public Instruction visitCallStatement(CallStatement callStatement, Void __) {
		visit(callStatement.callExpression);

		// discard return value if it exists
		int resultSize = callStatement.callExpression.getCalleeDefinition().getReturnType().wordSize;
		if(resultSize != 0) assembler.emitPop(0, resultSize).addComment("discard return value", false);

		return null;
	}

	@Override
	public Instruction visitReturnStatement(ReturnStatement returnStatement, Void __) {
		// leave the return value on the stack, the RETURN instruction is emitted in visitFunction
		visit(returnStatement.returnValue);
		return null;
	}
	@Override
	public Instruction visitCompoundStatement(CompoundStatement compoundStatement, Void __) {
		// TODO implement (task 3.2)
		// This method is responsible for handling a compound statement which contains multiple statements.
		// It needs to generate code for each individual statement in the compound statement and ensure that
		// all local variables and stack state are properly managed.

		int offsetBefore = assembler.getNextOffset();

		// Visit each statement within the compound statement
		for (Statement statement : compoundStatement.statements) {
			visit(statement);
		}

		// Reset the next offset to the position before entering the compound statement
		assembler.resetNextOffset(offsetBefore);

		return null;
	}

	@Override
	public Instruction visitSwitchStatement(SwitchStatement switchCaseStatement, Void __) {
		// TODO implement (task 3.6)
		// This method is responsible for handling a switch statement.
		// It needs to evaluate the switch condition and then handle each case or default branch.

		int firstOffset = assembler.getNextOffset();

		// Create a list to store jump instructions that need backpatching
		List<Instruction> caseEndJumps = new ArrayList<>();

		// Evaluate the switch condition expression
		visit(switchCaseStatement.condition);

		// Set the next offset to include the length of the switch condition's word size
		assembler.setNextOffset(firstOffset + switchCaseStatement.condition.getType().wordSize);

		// Visit each case in the switch statement and generate instructions for them
		for (Case currCase : switchCaseStatement.cases) {
			// Load the switch condition into a register
			assembler.loadAddress(Register.LB, firstOffset);
			assembler.loadFromStackAddress(1);

			// Call the visit method on the current case to handle its body
			Instruction endJump = visit(currCase);

			// Store this jump instruction for backpatching later
			caseEndJumps.add(endJump);
		}

		// If there is a default case, visit it and store the jump instruction
		if (!switchCaseStatement.defaults.isEmpty()) {
			visit(switchCaseStatement.defaults.get(0));
		}

		// Determine the exit address of the switch statement
		int exitAddress = assembler.getNextInstructionAddress();

		// Backpatch all the jump instructions to point to the exit address
		for (Instruction jumpInstruction : caseEndJumps) {
			assembler.backPatchJump(jumpInstruction, exitAddress);
		}

		// Reset the next offset back to before the switch statement was processed
		assembler.resetNextOffset(firstOffset);

		return null;
	}

	@Override
	public Instruction visitCase(Case namedCase, Void __) {
		// TODO implement (task 3.6)
		// This method is responsible for handling a single case within a switch statement.
		// It needs to compare the case condition with the switch condition and execute the associated body.

		// Load the integer value of the case condition
		assembler.loadIntegerValue(namedCase.getCondition());

		// Compare it with the current switch condition
		assembler.emitIntegerComparison(Comparison.EQUAL);

		// Create a jump instruction that jumps over to the next case if the conditions are not equal
		Instruction jumpNextCase = assembler.emitConditionalJump(false, -1);

		// Determine the next offset before entering the case body
		int nextOffset = assembler.getNextOffset();

		// Visit and generate code for the body of this case
		visit(namedCase.body);

		// Reset the next offset back to the position where we started handling the case
		assembler.resetNextOffset(nextOffset);

		// Create a jump instruction that exits the switch statement after handling this case
		Instruction jumpSwitchEnd = assembler.emitJump(0);

		// Backpatch the jump over to the current instruction (next case)
		assembler.backPatchJump(jumpNextCase, assembler.getNextInstructionAddress());

		return jumpSwitchEnd;
	}

	@Override
	public Instruction visitDefault(Default defaultCase, Void __) {
		// TODO implement (task 3.6)
		// This method is responsible for handling a default case within a switch statement.
		// It needs to execute the body of the default case if none of the other cases match.

		int nextOffset = assembler.getNextOffset();

		// Visit and generate code for the body of this default case
		visit(defaultCase.body);

		// Reset the next offset back to the position where we started handling the default case
		assembler.resetNextOffset(nextOffset);

		return null;
	}

	@Override
	public Instruction visitMatrixMultiplication(MatrixMultiplication matrixMultiplication, Void __) {
		visit(matrixMultiplication.leftOperand);
		visit(matrixMultiplication.rightOperand);

		StructType lType = (StructType) matrixMultiplication.leftOperand.getType();
		StructType rType = (StructType) matrixMultiplication.rightOperand.getType();

		if(!(lType instanceof MatrixType) || !(rType instanceof MatrixType))
			throw new InternalCompilerError("Matrix multiplication involving vectors is no longer supported");

		MatrixType lMat = (MatrixType) lType;
		MatrixType rMat = (MatrixType) rType;

		assembler.loadIntegerValue(lMat.rows);
		assembler.loadIntegerValue(lMat.cols);
		assembler.loadIntegerValue(rMat.cols);

		if(lType.elementType.equals(IntType.instance))
			assembler.emitIntegerMatrixMultiplication();
		else
			assembler.emitFloatMatrixMultiplication();

		return null;
	}

	@Override
	public Instruction visitDotProduct(DotProduct dotProduct, Void __) {
		VectorType vectorType = (VectorType) dotProduct.leftOperand.getType();

		visit(dotProduct.leftOperand);
		visit(dotProduct.rightOperand);

		assembler.loadIntegerValue(1);
		assembler.loadIntegerValue(vectorType.dimension);
		assembler.loadIntegerValue(1);

		// a dot product is basically a matrix multiplication of one vector with the other's transposed form
		if(vectorType.elementType.equals(IntType.instance))
			assembler.emitIntegerMatrixMultiplication();
		else
			assembler.emitFloatMatrixMultiplication();

		return null;
	}

	private void visitArithmeticOperator(BinaryExpression node, boolean allowLeftStruct, boolean allowRightStruct, boolean allowBothStruct, Primitive intPrimitive, Primitive floatPrimitive) {
		Type resultType = node.getType();
		Type leftType = node.leftOperand.getType();
		Type rightType = node.rightOperand.getType();
		int lSize = leftType.wordSize;
		int rSize = rightType.wordSize;

		// evaluate operands
		visit(node.leftOperand);
		visit(node.rightOperand);

		if(resultType.equals(IntType.instance)) {
			assembler.callPrimitive(intPrimitive);
			return;
		}

		if(resultType.equals(FloatType.instance)) {
			assembler.callPrimitive(floatPrimitive);
			return;
		}

		if(leftType instanceof StructType && rightType instanceof StructType) {
			if(!allowBothStruct)
				throw new InternalCompilerError(node.getClass().getSimpleName() + " does not support structures for both operands");

			boolean useIntPrimitive = ((StructType) leftType).elementType.equals(IntType.instance);

			// ..., left, right
			assembler.loadIntegerValue(0);
			// ..., left, right, i (0)
			int loopBegin = assembler.getNextInstructionAddress();

			// load operands
			assembler.loadValue(Register.ST, 1, -1);
			// ..., left, right, i, i
			assembler.loadAddress(Register.ST, -2 - lSize - rSize);
			// ..., left, right, i, i, &left
			assembler.emitIntegerAddition();
			// ..., left, right, i, &left[i]
			assembler.loadFromStackAddress(1);
			// ..., left, right, i, left[i]
			assembler.loadValue(Register.ST, 1, -2);
			// ..., left, right, i, left[i], i
			assembler.loadAddress(Register.ST, -3 - rSize);
			// ..., left, right, i, left[i], i, &right
			assembler.emitIntegerAddition();
			// ..., left, right, i, left[i], &right[i]
			assembler.loadFromStackAddress(1);
			// ..., left, right, i, left[i], right[i]

			// combine and store
			assembler.callPrimitive(useIntPrimitive ? intPrimitive : floatPrimitive);
			// ..., left, right, i, elem
			assembler.loadValue(Register.ST, 1, -2);
			// ..., left, right, i, elem, i
			assembler.loadAddress(Register.ST, -3 - lSize - rSize);
			// ..., left, right, i, elem, i, &left
			assembler.emitIntegerAddition();
			// ..., left, right, i, elem, &left[i]
			assembler.storeToStackAddress(1);
			// ..., left, right, i

			// increment and check
			assembler.emitIncrement();
			// ..., left, right, i+1
			assembler.loadValue(Register.ST, 1, -1);
			// ..., left, right, i+1, i+1
			assembler.loadIntegerValue(lSize);
			// ..., left, right, i+1, i+1, size
			assembler.emitIntegerComparison(Comparison.LESS);
			// ..., left, right, i+1, bool
			assembler.emitConditionalJump(true, loopBegin);
			// ..., left, right, i+1
			assembler.emitPop(0, 1 + rSize);
			// ..., result

			return;
		}

		if(leftType instanceof StructType) {
			if(!allowLeftStruct)
				throw new InternalCompilerError(node.getClass().getSimpleName() + " does not support structures for its left operand");

			boolean useIntPrimitive = rightType.equals(IntType.instance);

			// ..., struct, num
			assembler.loadIntegerValue(0);
			// ..., struct, num, i (0)
			int loopStart = assembler.getNextInstructionAddress();

			// load operands
			assembler.loadValue(Register.ST, 1, -1);
			// ..., struct, num, i, i
			assembler.loadAddress(Register.ST, -3 - lSize);
			// ..., struct, num, i, i, &struct
			assembler.emitIntegerAddition();
			// ..., struct, num, i, &struct[i]
			assembler.loadFromStackAddress(1);
			// ..., struct, num, i, struct[i]
			assembler.loadValue(Register.ST, 1, -3);
			// ..., struct, num, i, struct[i], num

			// combine and store
			assembler.callPrimitive(useIntPrimitive ? intPrimitive : floatPrimitive);
			// ..., struct, num, i, elem
			assembler.loadValue(Register.ST, 1, -2);
			// ..., struct, num, i, elem, i
			assembler.loadAddress(Register.ST, -4 - lSize);
			// ..., struct, num, i, elem, i, &struct
			assembler.emitIntegerAddition();
			// ..., struct, num, i, elem, &struct[i]
			assembler.storeToStackAddress(1);
			// ..., struct, num, i

			// increment and check
			assembler.emitIncrement();
			// ..., struct, num, i+1
			assembler.loadValue(Register.ST, 1, -1);
			// ..., struct, num, i+1, i+1
			assembler.loadIntegerValue(lSize);
			// ..., struct, num, i+1, i+1, size
			assembler.emitIntegerComparison(Comparison.LESS);
			// ..., struct, num, i+1, bool
			assembler.emitConditionalJump(true, loopStart);
			// ..., struct, num, i+1
			assembler.emitPop(0, 2);
			// ..., struct

			return;
		}

		if(rightType instanceof StructType) {
			if(!allowRightStruct)
				throw new InternalCompilerError(node.getClass().getSimpleName() + " does not support structures for its right operand");

			boolean useIntPrimitive = leftType.equals(IntType.instance);

			// ..., num, struct
			assembler.loadIntegerValue(0);
			// ..., num, struct, i (0)
			int loopStart = assembler.getNextInstructionAddress();

			// load operands
			assembler.loadValue(Register.ST, 1, -1);
			// ..., num, struct, i, i
			assembler.loadAddress(Register.ST, -2 - rSize);
			// ..., num, struct, i, i, &struct
			assembler.emitIntegerAddition();
			// ..., num, struct, i, &struct[i]
			assembler.loadFromStackAddress(1);
			// ..., num, struct, i, struct[i]
			assembler.loadValue(Register.ST, 1, -3 - rSize);
			// ..., num, struct, i, struct[i], num

			// combine and store
			assembler.callPrimitive(useIntPrimitive ? intPrimitive : floatPrimitive);
			// ..., num, struct, i, elem
			assembler.loadValue(Register.ST, 1, -2);
			// ..., num, struct, i, elem, i
			assembler.loadAddress(Register.ST, -3 - rSize);
			// ..., num, struct, i, elem, i, &struct
			assembler.emitIntegerAddition();
			// ..., num, struct, i, elem, &struct[i]
			assembler.storeToStackAddress(1);
			// ..., num, struct, i

			// increment and check
			assembler.emitIncrement();
			// ..., num, struct, i+1
			assembler.loadValue(Register.ST, 1, -1);
			// ..., num, struct, i+1, i+1
			assembler.loadIntegerValue(rSize);
			// ..., num, struct, i+1, i+1, size
			assembler.emitIntegerComparison(Comparison.LESS);
			// ..., num, struct, i+1, bool
			assembler.emitConditionalJump(true, loopStart);
			// ..., num, struct, i+1
			assembler.emitPop(0, 1);
			// ..., num, struct
			assembler.emitPop(rSize, 1);
			// ..., struct

			return;
		}

		throw new InternalCompilerError("How did we even get here?");
	}

	@Override
	public Instruction visitAddition(Addition addition, Void __) {
		visitArithmeticOperator(addition, false, false, true, Primitive.addI, Primitive.addF);
		return null;
	}

	@Override
	public Instruction visitSubtraction(Subtraction subtraction, Void __) {
		visitArithmeticOperator(subtraction, false, false, true, Primitive.subI, Primitive.subF);
		return null;
	}

	@Override
	public Instruction visitMultiplication(Multiplication multiplication, Void __) {
		visitArithmeticOperator(multiplication, true, true, true, Primitive.mulI, Primitive.mulF);
		return null;
	}

	@Override
	public Instruction visitDivision(Division division, Void __) {
		visitArithmeticOperator(division, false, false, false, Primitive.divI, Primitive.divF);
		return null;
	}

	@Override
	public Instruction visitExponentiation(Exponentiation exponentiation, Void __) {
		visitArithmeticOperator(exponentiation, false, false, false, Primitive.powInt, Primitive.powFloat);
		return null;
	}

	@Override
	public Instruction visitCompare(Compare compare, Void __) {
		visit(compare.leftOperand);
		visit(compare.rightOperand);
		if(compare.leftOperand.getType().equals(IntType.instance))
			assembler.emitIntegerComparison(compare.comparator);
		else
			assembler.emitFloatComparison(compare.comparator);
		return null;
	}

	@Override
	public Instruction visitAnd(And and, Void __) {
		// TODO implement (task 3.1)

		// This method is responsible for handling a logical AND operation.
		// It needs to evaluate both operands of the AND expression and then perform the logical AND on them.

		// Step 1: Evaluate the left operand
		visit(and.leftOperand);

		// Step 2: Evaluate the right operand
		visit(and.rightOperand);

		// Step 3: Emit the instruction for logical AND
		assembler.emitLogicalAnd();

		// The result of the AND operation is now available on the stack, and we can return null since there are no further instructions needed.
		return null;
	}

	@Override
	public Instruction visitOr(Or or, Void __) {
		visit(or.leftOperand);
		visit(or.rightOperand);
		assembler.emitLogicalOr();
		return null;
	}

	@Override
	public Instruction visitMatrixTranspose(MatrixTranspose matrixTranspose, Void __) {
		MatrixType type = (MatrixType) matrixTranspose.operand.getType();
		visit(matrixTranspose.operand);

		// transposition of a single row or column is a no-op
		if(type.cols <= 1 || type.rows <= 1) return null;
		assembler.loadIntegerValue(type.rows);
		assembler.loadIntegerValue(type.cols);
		assembler.emitMatrixTranspose();
		return null;
	}

	@Override
	public Instruction visitMatrixRows(MatrixRows rows, Void __) {
		MatrixType type = (MatrixType) rows.operand.getType();
		assembler.loadIntegerValue(type.rows).addComment("matrix rows", false);
		return null;
	}

	@Override
	public Instruction visitMatrixCols(MatrixCols cols, Void __) {
		MatrixType type = (MatrixType) cols.operand.getType();
		assembler.loadIntegerValue(type.cols).addComment("matrix cols", false);
		return null;
	}

	@Override
	public Instruction visitVectorDimension(VectorDimension vectorDimension, Void __) {
		VectorType type = (VectorType) vectorDimension.operand.getType();
		assembler.loadIntegerValue(type.dimension).addComment("vector dim", false);
		return null;
	}

	@Override
	public Instruction visitUnaryMinus(UnaryMinus unaryMinus, Void __) {
		// TODO implement (task 3.1)

		// This method is responsible for handling a unary minus operation.
		// It needs to evaluate the operand and then apply the negation based on whether it's an integer or float.

		// Step 1: Evaluate the operand of the unary minus
		visit(unaryMinus.operand);

		// Step 2: Determine the type of the operand and emit the appropriate negation instruction
		if (unaryMinus.operand.getType().equals(FloatType.instance)) {
			// If the operand is a float, emit the float negation instruction
			assembler.emitFloatNegation();
		} else {
			// Otherwise, emit the integer negation instruction
			assembler.emitIntegerNegation();
		}

		// The unary minus operation is now complete. Return null to indicate that no further instructions are needed.
		return null;
	}

	@Override
	public Instruction visitNot(Not not, Void __) {
		visit(not.operand);
		assembler.emitLogicalNot();
		return null;
	}

	@Override
	public Instruction visitCallExpression(CallExpression callExpression, Void __) {
		// TODO implement (task 3.1)

		// This method is responsible for handling a function call expression.
		// It needs to evaluate all actual parameters and then emit the function call instruction.

		// Step 1: Evaluate each actual parameter of the call expression
		// Ensure that all parameters are evaluated before the function call is made, as the order matters in some cases (e.g., variable arguments).
		for (Expression parameter : callExpression.actualParameters) {
			// Visit each parameter to evaluate its value and place it on the stack if necessary
			visit(parameter);

			// If a parameter is an identifier or other expression that requires evaluation before being passed, this step ensures it's done.
		}

		// Step 2: Emit the function call instruction
		// The callee definition of the function being called is obtained and used to perform the function call
		assembler.emitFunctionCall(callExpression.getCalleeDefinition());

		// Since the function call may return a value, ensure that any necessary cleanup or handling of the return value is done appropriately.

		// Return null since this method only generates instructions for evaluating the parameters and making the call; further handling is typically managed elsewhere
		return null;
	}

	@Override
	public Instruction visitElementSelect(ElementSelect elementSelect, Void __) {
		StructType structType = (StructType) elementSelect.structExpression.getType();
		Type resultType = elementSelect.getType();
		int structSize = structType.wordSize;
		int resultSize = resultType.wordSize;
		int upperBound = structSize / resultSize;

		// ...
		visit(elementSelect.structExpression);
		// ..., struct
		assembler.loadAddress(Register.ST, -structSize);
		// ..., struct, &struct
		visit(elementSelect.indexExpression);
		assembler.emitBoundsCheck(0, upperBound);
		// ..., struct, &struct, index
		if(resultSize != 1) {
			assembler.loadIntegerValue(resultSize);
			assembler.emitIntegerMultiplication();
		}
		assembler.emitIntegerAddition();
		// ..., struct, &struct[index]
		assembler.loadFromStackAddress(resultSize);
		// ..., struct, result
		assembler.emitPop(resultSize, structSize);
		// ..., result
		return null;
	}

	@Override
	public Instruction visitRecordElementSelect(RecordElementSelect recordElementSelect, Void __) {
		Type elementType = recordElementSelect.getType();
		RecordType recordType = (RecordType) recordElementSelect.recordExpression.getType();
		int offset = recordType.typeDeclaration.getElementOffset(recordElementSelect.elementName);

		// ...
		visit(recordElementSelect.recordExpression);
		// ..., record
		assembler.loadAddress(Register.ST, -recordType.wordSize + offset);
		// ..., record, &record[offset]
		assembler.loadFromStackAddress(elementType.wordSize);
		// ..., record, result
		assembler.emitPop(elementType.wordSize, recordType.wordSize);
		// ..., result
		return null;
	}

	@Override
	public Instruction visitSubMatrix(SubMatrix subMatrix, Void __) {
		MatrixType matrix = (MatrixType) subMatrix.structExpression.getType();
		MatrixType result = (MatrixType) subMatrix.getType();
		int matSize = matrix.wordSize;
		int resSize = result.wordSize;

		int rowStartOffset = subMatrix.getRowStartOffset();
		int colStartOffset = subMatrix.getColStartOffset();

		// ...
		visit(subMatrix.structExpression);
		// ..., mat
		assembler.emitPush(resSize);
		// ..., mat, res
		assembler.loadAddress(Register.ST, -matSize - resSize);
		// ..., mat, res, &mat
		visit(subMatrix.rowBaseIndexExpression);
		if(rowStartOffset != 0) {
			assembler.loadIntegerValue(rowStartOffset);
			assembler.emitIntegerAddition();
		}
		assembler.emitBoundsCheck(0, matrix.rows - result.rows + 1);
		// ..., mat, res, &mat, minRow
		assembler.loadIntegerValue(matrix.cols);
		assembler.emitIntegerMultiplication();
		assembler.emitIntegerAddition();
		// ..., mat, res, &mat[minRow][0]
		visit(subMatrix.colBaseIndexExpression);
		if(colStartOffset != 0) {
			assembler.loadIntegerValue(colStartOffset);
			assembler.emitIntegerAddition();
		}
		assembler.emitBoundsCheck(0, matrix.cols - result.cols + 1);
		// ..., mat, res, &mat[minRow][0], minCol
		assembler.emitIntegerAddition();
		// ..., mat, res, &mat[minRow][minCol]
		assembler.loadAddress(Register.ST, -resSize - 1);
		// ..., mat, res, &mat[minRow][minCol], &res
		assembler.loadIntegerValue(0);
		// ..., mat, res, &mat[minRow][minCol], &res, i (0)

		// loop header
		int loopStart = assembler.getNextInstructionAddress();
		// ..., mat, res, srcPtr, dstPtr, i
		assembler.loadValue(Register.ST, 1, -1);
		// ..., mat, res, srcPtr, dstPtr, i, i
		Instruction jumpEnd = assembler.emitConditionalJump(result.rows, -1); // break if i == result.rows
		// ..., mat, res, srcPtr, dstPtr, i

		// copy row
		assembler.loadValue(Register.ST, 1, -3);
		// ..., mat, res, srcPtr, dstPtr, i, srcPtr
		assembler.loadFromStackAddress(result.cols);
		// ..., mat, res, srcPtr, dstPtr, i, row
		assembler.loadValue(Register.ST, 1, -2 - result.cols);
		// ..., mat, res, srcPtr, dstPtr, i, row, dstPtr
		assembler.storeToStackAddress(result.cols);
		// ..., mat, res, srcPtr, dstPtr, i

		// increment
		assembler.loadValue(Register.ST, 1, -3);
		// ..., mat, res, srcPtr, dstPtr, i, srcPtr
		assembler.loadIntegerValue(matrix.cols);
		// ..., mat, res, srcPtr, dstPtr, i, srcPtr, matCols
		assembler.emitIntegerAddition();
		// ..., mat, res, srcPtr, dstPtr, i, srcPtr'
		assembler.storeValue(Register.ST, 1, -4);
		// ..., mat, res, srcPtr', dstPtr, i
		assembler.loadValue(Register.ST, 1, -2);
		// ..., mat, res, srcPtr', dstPtr, i, dstPtr
		assembler.loadIntegerValue(result.cols);
		// ..., mat, res, srcPtr', dstPtr, i, dstPtr, resCols
		assembler.emitIntegerAddition();
		// ..., mat, res, srcPtr', dstPtr, i, dstPtr'
		assembler.storeValue(Register.ST, 1, -3);
		// ..., mat, res, srcPtr', dstPtr', i
		assembler.emitIncrement();
		// ..., mat, res, srcPtr', dstPtr', i'

		assembler.emitJump(loopStart);
		int loopEnd = assembler.getNextInstructionAddress();
		assembler.backPatchJump(jumpEnd, loopEnd);

		// ..., mat, res, srcPtr, dstPtr, i
		assembler.emitPop(0, 3);
		// ..., mat, res
		assembler.emitPop(resSize, matSize);
		// ..., res

		return null;
	}

	@Override
	public Instruction visitSubVector(SubVector subVector, Void __) {
		// TODO implement (task 3.7)

		// This method is responsible for handling a subvector expression.
		// It extracts a subset of elements from the given vector and returns them as a new vector.

		int initOffset = subVector.getStartOffset();

		// Get the size of the original vector and the size of the subvector
		int vecSize = subVector.structExpression.getType().wordSize;
		int subVecSize = subVector.getType().wordSize;

		// Step 1: Evaluate the structural expression (the original vector)
		visit(subVector.structExpression);

		// Load the address of the original vector from the stack
		assembler.loadAddress(Register.ST, -vecSize);

		// Step 2: Evaluate the base index expression for the starting position of the subvector
		visit(subVector.baseIndexExpression);

		// Add the initial offset to the base index to get the starting address in the original vector
		assembler.loadIntegerValue(initOffset);
		assembler.emitIntegerAddition();

		// Ensure that the starting index is within bounds
		assembler.emitBoundsCheck(0, vecSize - subVecSize + 1);

		// Add the resulting address of the start of the subvector to the stack
		assembler.emitIntegerAddition();

		// Load the elements from the original vector starting at the calculated offset and for the specified length (subvector size)
		assembler.loadFromStackAddress(subVecSize);

		// Pop the loaded elements into a new subvector
		assembler.emitPop(subVecSize, vecSize);

		// The subvector is now available on the stack with the required elements
		return null;
	}

	@Override
	public Instruction visitStructureInit(StructureInit structureInit, Void __) {
		structureInit.elements.forEach(this::visit);
		return null;
	}

	@Override
	public Instruction visitStringValue(StringValue stringValue, Void __) {
		assembler.loadStringValue(stringValue.value);
		return null;
	}

	@Override
	public Instruction visitBoolValue(BoolValue boolValue, Void __) {
		// TODO implement (task 3.1)

		// This method is responsible for handling a boolean value expression.
		// It needs to load the given boolean value onto the stack.

		// Step 1: Load the boolean value onto the stack
		// Use the assembler's `loadBooleanValue` method to place the boolean value on the stack
		assembler.loadBooleanValue(boolValue.value);

		// Return null since this method only generates instructions for loading the boolean value; further handling is typically managed elsewhere
		return null;
	}

	@Override
	public Instruction visitIntValue(IntValue intValue, Void __) {
		assembler.loadIntegerValue(intValue.value);
		return null;
	}

	@Override
	public Instruction visitFloatValue(FloatValue floatValue, Void __) {
		assembler.loadFloatValue(floatValue.value);
		return null;
	}

	@Override
	public Instruction visitIdentifierReference(IdentifierReference identifierReference, Void __) {
		Declaration decl = identifierReference.getDeclaration();
		int wordSize = decl.getType().wordSize;
		int offset = decl.getLocalBaseOffset();
		assembler.loadLocalValue(wordSize, offset).addName(identifierReference.name)
		/*/.addComment("load identifier '" + identifierReference.name + "'")/**/;
		return null;
	}

	@Override
	public Instruction visitSelectExpression(SelectExpression exp, Void __) {
		// TODO implement (task 3.4)

		// This method is responsible for handling a select expression, which acts like a ternary operator or an if-else statement.
		// It evaluates the condition and selects between two cases based on whether the condition is true or false.

		// Step 1: Evaluate the Condition
		visit(exp.condition);

		// Step 2: Emit Conditional Jump to Handle 'Else' Case
		// Create a jump instruction that will skip to the end of the then branch if the condition is false
		Instruction condJumpToElse = assembler.emitConditionalJump(false, -1);

		// Step 3: Evaluate and Execute the True Case (Then Branch)
		visit(exp.trueCase);

		// Step 4: Emit Jump to End of Selection Block
		// Create a jump instruction that will skip to the end of the else branch if the condition is not false
		Instruction jumpToEnd = assembler.emitJump(-1);

		// Step 5: Save Address for Backpatching (Then Branch)
		int thenEndAddress = assembler.getNextInstructionAddress();

		// Step 6: Backpatch the Jump from Conditional Jump to Else Case
		// Replace the target of the conditional jump with the address of the end of the then branch
		assembler.backPatchJump(condJumpToElse, thenEndAddress);

		// Step 7: Evaluate and Execute the False Case (Else Branch)
		visit(exp.falseCase);

		// Step 8: Save Address for Backpatching (Else Branch)
		int elseEndAddress = assembler.getNextInstructionAddress();

		// Step 9: Backpatch the Jump to End of Selection Block
		// Replace the target of the jump from the end of then or else branches with the address of the end of the selection block
		assembler.backPatchJump(jumpToEnd, elseEndAddress);

		// Return null since this method only generates instructions for handling the select expression; further handling is typically managed elsewhere
		return null;
	}
}
