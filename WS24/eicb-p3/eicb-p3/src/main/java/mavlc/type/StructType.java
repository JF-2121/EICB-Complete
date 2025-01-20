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

/** Super type of matrix- and vector-types. */
public abstract class StructType extends MemberType {
	public final NumericType elementType;
	
	/**
	 * @param elementType The type of elements.
	 * @param elementCount The total number of elements.
	 */
	StructType(NumericType elementType, int elementCount) {
		super(elementType.wordSize * elementCount);
		this.elementType = elementType;
	}
}
