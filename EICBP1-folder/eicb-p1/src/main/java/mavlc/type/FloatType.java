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

/** MAVL floating-point value type. */
public class FloatType extends NumericType {
	public static final FloatType instance = new FloatType();
	
	private FloatType() { }
	
	@Override
	public String toString() { return "float"; }
}
