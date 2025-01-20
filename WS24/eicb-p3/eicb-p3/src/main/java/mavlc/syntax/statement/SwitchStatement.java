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
import mavlc.syntax.expression.Expression;

import java.util.List;
import java.util.Objects;

/**
 * AST node representing a switch-case-statement.
 */
public class SwitchStatement extends Statement {
	
	public final Expression condition;
	public final List<Case> cases;
	public final List<Default> defaults;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param condition Condition expression to switch over.
	 * @param cases The list of named cases of this switch statement.
	 * @param defaults The list of default cases of this switch statement.
	 */
	public SwitchStatement(SourceLocation sourceLocation, Expression condition, List<Case> cases, List<Default> defaults) {
		super(sourceLocation);
		this.condition = condition;
		this.cases = cases;
		this.defaults = defaults;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitSwitchStatement(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		SwitchStatement that = (SwitchStatement) o;
		return Objects.equals(condition, that.condition) &&
				Objects.equals(cases, that.cases) &&
				Objects.equals(defaults, that.defaults);
	}
	
	@Override public int hashCode() {
		return Objects.hash(condition, cases, defaults);
	}
}
	
