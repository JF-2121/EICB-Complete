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

/** MAVL boolean type. */
public class BoolType extends PrimitiveType {
	public static final BoolType instance = new BoolType();
	
	private BoolType() { }
	
	@Override
	public String toString() { return "bool"; }
}
