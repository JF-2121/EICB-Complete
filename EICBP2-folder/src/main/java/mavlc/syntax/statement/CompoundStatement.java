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
package mavlc.syntax.statement;

import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.SourceLocation;

import java.util.List;
import java.util.Objects;

/**
 * AST node representing a block of statements.
 */
public class CompoundStatement extends Statement {
	
	public final List<Statement> statements;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param statements The list of statements within this code block.
	 */
	public CompoundStatement(SourceLocation sourceLocation, List<Statement> statements) {
		super(sourceLocation);
		this.statements = statements;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitCompoundStatement(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		CompoundStatement that = (CompoundStatement) o;
		return Objects.equals(statements, that.statements);
	}
	
	@Override public int hashCode() {
		return Objects.hash(statements);
	}
}
