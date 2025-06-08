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

/** Abstract super-class of all MAVL types. */
public abstract class Type {
	/** Size of this type on the MTAM in words. */
	public final int wordSize;
	
	protected Type(int wordSize) {
		this.wordSize = wordSize;
	}
	
	
	/** @return True if the type is a value type (everything but void). */
	public final boolean isValueType() { return this instanceof ValueType; }
	
	/** @return True if the type is a member type (everything but void and record). */
	public final boolean isMemberType() { return this instanceof MemberType; }
	
	/** @return True if the type is a structure type (vector or matrix). */
	public final boolean isStructType() { return this instanceof StructType; }
	
	/** @return True if the type is a primitive type (int, float or bool). */
	public final boolean isPrimitiveType() { return this instanceof PrimitiveType; }
	
	/** @return True if the type is a scalar type (int or float). */
	public final boolean isNumericType() { return this instanceof NumericType; }
	
	@Override public boolean equals(Object obj) {
		return obj != null
				&& obj.getClass() == this.getClass()
				&& obj.toString().equals(this.toString());
	}
}
