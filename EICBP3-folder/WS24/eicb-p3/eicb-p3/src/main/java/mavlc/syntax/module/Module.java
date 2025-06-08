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
package mavlc.syntax.module;

import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.function.Function;
import mavlc.syntax.record.RecordTypeDeclaration;

import java.util.List;
import java.util.Objects;

/**
 * AST node representing a MAVL module.
 */
public class Module extends AstNode {
	
	public final List<Function> functions;
	public final List<RecordTypeDeclaration> records;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param functions The list of declared functions.
	 * @param records The list of declared records.
	 *
	 */
	public Module(SourceLocation sourceLocation, List<Function> functions, List<RecordTypeDeclaration> records) {
		super(sourceLocation);
		this.functions = functions;
		this.records = records;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitModule(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		Module module = (Module) o;
		return Objects.equals(functions, module.functions) &&
				Objects.equals(records, module.records);
	}
	
	@Override public int hashCode() {
		return Objects.hash(functions, records);
	}
}
