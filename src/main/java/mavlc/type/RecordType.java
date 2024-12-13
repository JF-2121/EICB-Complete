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
package mavlc.type;

import mavlc.syntax.record.RecordTypeDeclaration;

import java.util.Objects;

/** Record type. */
public class RecordType extends ValueType {
	public final String name;
	public final RecordTypeDeclaration typeDeclaration;
	
	/**
	 * @param name The name of the record type.
	 * @param typeDeclaration the record type typeDeclaration of the record type.
	 */
	public RecordType(String name, RecordTypeDeclaration typeDeclaration) {
		super(typeDeclaration.elements.stream().mapToInt(elem -> elem.getType().wordSize).sum());
		this.name = name;
		this.typeDeclaration = typeDeclaration;
	}
	
	@Override
	public String toString() {
		return name;
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		RecordType that = (RecordType) o;
		return Objects.equals(name, that.name) &&
				Objects.equals(typeDeclaration, that.typeDeclaration);
	}
	
	@Override public int hashCode() {
		return Objects.hash(name, typeDeclaration);
	}
}
