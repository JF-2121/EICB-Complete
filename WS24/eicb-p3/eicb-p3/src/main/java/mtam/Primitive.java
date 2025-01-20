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
import mtam.interpreter.MachineState;

import java.util.Arrays;
import java.util.Optional;

public enum Primitive {
	nop(0),
	err(1),
	
	not(2),
	and(3),
	or(4),
	
	succ(5),
	pred(6),
	
	negI(7),
	addI(8),
	subI(9),
	mulI(10),
	divI(11),
	modI(12),
	
	eqI(13),
	neI(14),
	ltI(15),
	leI(16),
	gtI(17),
	geI(18),
	
	negF(19),
	addF(20),
	subF(21),
	mulF(22),
	divF(23),
	
	eqF(24),
	neF(25),
	ltF(26),
	leF(27),
	gtF(28),
	geF(29),
	
	readImage(30),
	writeImage(31),
	
	readIM64(32),
	readIM16(33),
	readIM9(34),
	writeIM64(35),
	writeIM16(36),
	writeIM9(37),
	
	readFM64(38),
	readFM16(39),
	readFM9(40),
	writeFM64(41),
	writeFM16(42),
	writeFM9(43),
	
	powInt(44),
	powFloat(45),
	sqrtInt(46),
	sqrtFloat(47),
	
	printInt(48),
	printFloat(49),
	printBool(50),
	printString(51),
	printLine(52),
	
	readInt(53),
	readFloat(54),
	readBool(55),
	
	int2float(56),
	float2int(57),
	
	matMulI(58),
	matMulF(59),
	matTranspose(60);
	
	public static final int count = values().length;
	public static final int baseAddress = MachineState.maxInstructions;
	
	public final int displacement;
	
	Primitive(int displacement) {
		this.displacement = displacement;
	}
	
	public static Primitive fromDisplacement(int displacement) throws ExecutionException {
		Optional<Primitive> opt = Arrays.stream(values()).filter(r -> r.displacement == displacement).findFirst();
		if(opt.isPresent())
			return opt.get();
		throw new ExecutionException(ErrorCode.invalidAddress, "Unable to call primitive with displacement " + displacement, -1);
	}
}
