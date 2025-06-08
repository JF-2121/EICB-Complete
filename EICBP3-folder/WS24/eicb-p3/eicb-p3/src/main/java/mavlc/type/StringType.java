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

/** MAVL string value type. */
public class StringType extends MemberType {
	public static final StringType instance = new StringType();
	
	private StringType() {
		super(1);
	}
	
	@Override
	public String toString() { return "string"; }
}
