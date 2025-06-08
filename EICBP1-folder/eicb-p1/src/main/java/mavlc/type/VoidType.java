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

/** MAVL void type. */
public class VoidType extends Type {
	public static final VoidType instance = new VoidType();
	
	private VoidType() {
		super(0);
	}
	
	@Override
	public String toString() { return "void"; }
}
