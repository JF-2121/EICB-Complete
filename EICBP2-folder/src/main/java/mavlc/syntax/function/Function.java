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
package mavlc.syntax.function;

import mavlc.errors.InternalCompilerError;
import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.statement.Statement;
import mavlc.syntax.type.TypeSpecifier;
import mavlc.type.Type;

import java.util.List;
import java.util.Objects;

/**
 * AST node representing a function.
 */
public class Function extends AstNode {
	
	public final String name;
	public final TypeSpecifier<?> returnTypeSpecifier;
	public final List<FormalParameter> parameters;
	public final List<Statement> body;
	
	protected Type returnType;
	
	protected Integer codeBaseOffset;
	
	/**
	 * @param sourceLocation The location in which the node was specified.
	 * @param name The name of the function.
	 * @param returnTypeSpecifier Return type specifier of the function.
	 * @param parameters A list of formal parameters.
	 * @param body A list of statements.
	 */
	public Function(SourceLocation sourceLocation, String name, TypeSpecifier<?> returnTypeSpecifier, List<FormalParameter> parameters, List<Statement> body) {
		super(sourceLocation);
		this.name = name;
		this.returnTypeSpecifier = returnTypeSpecifier;
		this.parameters = parameters;
		this.body = body;
	}
	
	/**
	 * Get a string representation of the function signature.
	 *
	 * @return The function signature.
	 */
	public String getSignature() {
		StringBuilder sb = new StringBuilder();
		sb.append("function ");
		if(returnTypeSpecifier == null)
			// if no return type specifier is present, this is one of the mocked runtime functions
			sb.append(getReturnType());
		else {
			sb.append(returnTypeSpecifier.dump());
		}
		sb.append(' ');
		sb.append(name);
		sb.append('(');
		boolean first = true;
		for(FormalParameter param : parameters) {
			if(!first) sb.append(", ");
			first = false;
			if(param.typeSpecifier != null)
				sb.append(param.dump());
			else {
				// parameter has no type specifier -> mock function
				sb.append(param.getType());
				sb.append(' ');
				sb.append(param.name);
			}
		}
		sb.append(')');
		return sb.toString();
	}
	
	/**
	 * Get the return type of the function.
	 *
	 * @return Function return type.
	 */
	public Type getReturnType() {
		if(returnType == null) throw new InternalCompilerError(this, "Return type of function has not been set");
		return returnType;
	}
	
	public void setReturnType(Type returnType) {
		this.returnType = returnType;
	}
	
	public boolean isReturnTypeSet() {
		return returnType != null;
	}
	
	/**
	 * Get the offset from the CB-register
	 * of the first instruction of this function.
	 *
	 * @return Code offset from the CB-register.
	 */
	public int getCodeBaseOffset() {
		if(codeBaseOffset == null)
			throw new InternalCompilerError("Code base offset of function " + name + " has not been set");
		return codeBaseOffset;
	}
	
	/**
	 * Set the offset from the CB-register
	 * of the first instruction of this function.
	 *
	 * @param codeBaseOffset Offset from the CB-register.
	 */
	public void setCodeBaseOffset(int codeBaseOffset) {
		this.codeBaseOffset = codeBaseOffset;
	}
	
	public boolean isCodeBaseOffsetSet() {
		return codeBaseOffset != null;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitFunction(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		Function function = (Function) o;
		return Objects.equals(name, function.name) &&
				Objects.equals(returnTypeSpecifier, function.returnTypeSpecifier) &&
				Objects.equals(parameters, function.parameters) &&
				Objects.equals(body, function.body) &&
				Objects.equals(returnType, function.returnType) &&
				Objects.equals(codeBaseOffset, function.codeBaseOffset);
	}
	
	@Override public int hashCode() {
		return Objects.hash(name, returnTypeSpecifier, parameters, body);
	}
}
