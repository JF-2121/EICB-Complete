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
package mtam;

import mtam.errors.ErrorCode;
import mtam.errors.ExecutionException;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

public enum Register {
	CB(0), // code base
	CT(1), // code top
	PB(2), // primitive base
	PT(3), // primitive top
	SB(4), // stack base
	ST(5), // stack top
	LB(6), // local base
	CP(7); // instruction pointer
	
	public static final int count = values().length;
	
	public static final List<Register> codeRegisters = Arrays.asList(CB, CT, CB, PT);
	public static final List<Register> stackRegisters = Arrays.asList(SB, ST, LB);
	
	public final int id;
	
	Register(int id) {
		this.id = id;
	}
	
	public static Register fromId(int id) throws ExecutionException {
		Optional<Register> opt = Arrays.stream(values()).filter(r -> r.id == id).findFirst();
		if(opt.isPresent())
			return opt.get();
		throw new ExecutionException(ErrorCode.malformedInstruction, "Invalid register id: " + id, -1);
	}
}
