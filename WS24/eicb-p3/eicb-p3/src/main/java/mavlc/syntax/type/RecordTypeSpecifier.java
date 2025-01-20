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
package mavlc.syntax.type;

import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.SourceLocation;
import mavlc.type.RecordType;

import java.util.Objects;

public class RecordTypeSpecifier extends TypeSpecifier<RecordType> {
	public final String recordTypeName;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param recordTypeName Name of the record type.
	 */
	public RecordTypeSpecifier(SourceLocation sourceLocation, String recordTypeName) {
		super(sourceLocation, RecordType.class);
		this.recordTypeName = recordTypeName;
	}
	
	@Override public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitRecordTypeSpecifier(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		RecordTypeSpecifier that = (RecordTypeSpecifier) o;
		return Objects.equals(recordTypeName, that.recordTypeName);
	}
	
	@Override public int hashCode() {
		return Objects.hash(recordTypeName);
	}
}
