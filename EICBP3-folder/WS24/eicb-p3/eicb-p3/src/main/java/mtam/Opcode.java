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
import java.util.Optional;

public enum Opcode {
	LOAD(0, true, true, true),
	LOADA(1, false, true, true),
	LOADI(2, true, false, false),
	LOADL(3, false, true, false),
	STORE(4, true, true, true),
	STOREI(5, true, false, false),
	CALL(6, false, true, true),
	CALLI(7, false, false, false),
	RETURN(8, true, true, false),
	PUSH(9, false, true, false),
	POP(10, true, true, false),
	JUMP(11, false, true, true),
	JUMPI(12, false, false, false),
	JUMPIF(13, true, true, true),
	HALT(14, false, false, false);
	
	public static final int count = values().length;
	
	public final int id;
	public final boolean hasN;
	public final boolean hasD;
	public final boolean hasR;
	
	Opcode(int id, boolean hasN, boolean hasD, boolean hasR) {
		this.id = id;
		this.hasN = hasN;
		this.hasD = hasD;
		this.hasR = hasR;
	}
	
	public static Opcode fromId(int id) throws ExecutionException {
		Optional<Opcode> opt = Arrays.stream(values()).filter(r -> r.id == id).findFirst();
		if(opt.isPresent())
			return opt.get();
		throw new ExecutionException(ErrorCode.malformedInstruction, "Invalid opcode id: " + id, -1);
	}
}
