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

import mavlc.errors.InternalCompilerError;

import java.util.Objects;

/**
 * Vector type.
 */
public class VectorType extends StructType {
	public final int dimension;
	
	/**
	 * @param elementType The type of elements in the vector type.
	 * @param dimension The number of elements in the vector type.
	 */
	public VectorType(NumericType elementType, int dimension) {
		super(elementType, dimension);
		if(dimension <= 0) throw new InternalCompilerError("Vector dimension must be strictly positive");
		this.dimension = dimension;
	}
	
	@Override
	public String toString() {
		return "vector<" + elementType + ">[" + dimension + "]";
	}
	
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		VectorType that = (VectorType) o;
		return dimension == that.dimension &&
				Objects.equals(elementType, that.elementType);
	}
	
	@Override public int hashCode() {
		return Objects.hash(dimension, elementType);
	}
}
