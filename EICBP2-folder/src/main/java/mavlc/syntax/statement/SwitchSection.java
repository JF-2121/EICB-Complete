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

import mavlc.syntax.AstNode;
import mavlc.syntax.SourceLocation;

import java.util.Objects;

/**
 * AST node representing a case or default-case.
 */
public abstract class SwitchSection extends AstNode {
	
	public final Statement body;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param body The statement that gets executed if the case's condition applies.
	 */
	public SwitchSection(SourceLocation sourceLocation, Statement body) {
		super(sourceLocation);
		this.body = body;
	}
	
	public final boolean isDefault() {
		return this instanceof Default;
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		SwitchSection that = (SwitchSection) o;
		return Objects.equals(body, that.body);
	}
	
	@Override public int hashCode() {
		return Objects.hash(body);
	}
}
