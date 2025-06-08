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
package mtam.interpreter;

import mavlc.type.*;
import mtam.errors.ErrorCode;
import mtam.errors.ExecutionException;

import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class Value {
	public static final Value zero = new Value(Type.unknown, 0);
	
	// TODO store the cycle number this value was created on (to highlight changes in interactive interpreter)?
	
	public final int bits;
	public final Type type;
	
	public Value(Type type, int bits) {
		this.bits = bits;
		this.type = type;
	}
	
	public Value(int value) {
		bits = value;
		type = Type.intT;
	}
	
	public Value(boolean value) {
		bits = value ? 1 : 0;
		type = Type.boolT;
	}
	
	public Value(float value) {
		bits = Float.floatToRawIntBits(value);
		type = Type.floatT;
	}

	private void checkType(Type... expected) {
		// We can't perform type checks on unknown types. Allow bit cast to support running programs without debug
		// information.
		if (type == Type.unknown) return;
		for (Type allowed : expected) {
			if (type == allowed) return;
		}

		// Illegal type conversion, report
		String expectedDesc = Stream.of(expected)
				.map(Enum::toString)
				.collect(Collectors.joining(", "));
		throw new ExecutionException(ErrorCode.typeMismatch, "Illegal conversion from " + type + ", expected " + expectedDesc);
	}

	public int asInt() {
		checkType(Type.boolT, Type.intT, Type.stringT, Type.cAddrT, Type.sAddrT);
		return bits;
	}
	
	public boolean asBool() {
		checkType(Type.boolT);
		assert(bits == 0 || bits == 1);

		return bits == 1;
	}
	
	public float asFloat() {
		checkType(Type.floatT);
		return Float.intBitsToFloat(bits);
	}
	
	public enum Type {
		unknown, intT, boolT, floatT, stringT, cAddrT, sAddrT;
		
		public final int id = ordinal();
		
		public static Type fromId(int id) {
			return values()[id];
		}

		public static Type fromMavl(mavlc.type.Type type) {
			mavlc.type.Type primitive;

			// Find the primitive type making up this variable declaration
			if (type.isStructType()) {
				primitive = ((StructType) type).elementType;
			} else {
				primitive = type;
			}

			if (primitive == IntType.instance) {
				return Type.intT;
			} else if (primitive == FloatType.instance) {
				return Type.floatT;
			} else if (primitive == BoolType.instance) {
				return Type.boolT;
			} else {
				return Type.unknown;
			}
		}
	}
}
