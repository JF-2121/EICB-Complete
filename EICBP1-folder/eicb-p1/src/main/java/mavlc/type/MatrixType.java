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

/** Matrix type. */
public class MatrixType extends StructType {
	public final int rows;
	public final int cols;
	
	/**
	 * @param elementType The type of the elements in the matrix type.
	 * @param rows The constant expression for the number of rows in the matrix type.
	 * @param cols The constant expression for the number of columns in the matrix type.
	 */
	public MatrixType(NumericType elementType, int rows, int cols) {
		super(elementType, rows * cols);
		if(rows <= 0 || cols <= 0) throw new InternalCompilerError("Matrix dimensions must be strictly positive");
		this.rows = rows;
		this.cols = cols;
	}
	
	@Override
	public String toString() {
		return "matrix<" + elementType + ">[" + rows + "][" + cols + "]";
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		MatrixType that = (MatrixType) o;
		return rows == that.rows &&
				cols == that.cols &&
				Objects.equals(elementType, that.elementType);
	}
	
	@Override public int hashCode() {
		return Objects.hash(rows, cols, elementType);
	}
}
