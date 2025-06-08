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
package mavlc.context_analysis;

import mavlc.syntax.SourceLocation;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.type.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * Dummy declarations of all functions defined in the
 * standard runtime environment.
 */
public class RuntimeFunctions {
	
	public static Map<String, Function> getRuntimeFunctions() {
		Map<String, Function> runtimeFunctions = new HashMap<>();
		
		IntType $int = IntType.instance;
		VoidType $void = VoidType.instance;
		BoolType $bool = BoolType.instance;
		FloatType $float = FloatType.instance;
		StringType $string = StringType.instance;
		
		MatrixType $matrixInt512 = new MatrixType($int, 512, 512);
		MatrixType $matrixInt64 = new MatrixType($int, 64, 64);
		MatrixType $matrixInt16 = new MatrixType($int, 16, 16);
		MatrixType $matrixInt9 = new MatrixType($int, 9, 9);
		
		MatrixType $matrixFloat64 = new MatrixType($float, 64, 64);
		MatrixType $matrixFloat16 = new MatrixType($float, 16, 16);
		MatrixType $matrixFloat9 = new MatrixType($float, 9, 9);
		
		/*
		 * Image IO
		 */
		addMockFunction(runtimeFunctions, "readImage", $matrixInt512, Primitive.readImage,
				mockParameter("filename", $string)
		);
		
		addMockFunction(runtimeFunctions, "writeImage", $void, Primitive.writeImage,
				mockParameter("filename", $string),
				mockParameter("image", $matrixInt512)
		);
		
		/*
		 * Read integer matrices
		 */
		addMockFunction(runtimeFunctions, "readIntMatrix64", $matrixInt64, Primitive.readIM64,
				mockParameter("filename", $string)
		);
		
		addMockFunction(runtimeFunctions, "readIntMatrix16", $matrixInt16, Primitive.readIM16,
				mockParameter("filename", $string)
		);
		
		addMockFunction(runtimeFunctions, "readIntMatrix9", $matrixInt9, Primitive.readIM9,
				mockParameter("filename", $string)
		);
		
		/*
		 * Write integer matrices
		 */
		addMockFunction(runtimeFunctions, "writeIntMatrix64", $void, Primitive.writeIM64,
				mockParameter("filename", $string),
				mockParameter("matrix", $matrixInt64)
		);
		
		addMockFunction(runtimeFunctions, "writeIntMatrix16", $void, Primitive.writeIM16,
				mockParameter("filename", $string),
				mockParameter("matrix", $matrixInt16)
		);
		
		addMockFunction(runtimeFunctions, "writeIntMatrix9", $void, Primitive.writeIM9,
				mockParameter("filename", $string),
				mockParameter("matrix", $matrixInt9)
		);
		
		/*
		 * Read floating point matrices
		 */
		addMockFunction(runtimeFunctions, "readFloatMatrix64", $matrixFloat64, Primitive.readFM64,
				mockParameter("filename", $string)
		);
		
		addMockFunction(runtimeFunctions, "readFloatMatrix16", $matrixFloat16, Primitive.readFM16,
				mockParameter("filename", $string)
		);
		
		addMockFunction(runtimeFunctions, "readFloatMatrix9", $matrixFloat9, Primitive.readFM9,
				mockParameter("filename", $string)
		);
		
		/*
		 * Write floating point matrices
		 */
		addMockFunction(runtimeFunctions, "writeFloatMatrix64", $void, Primitive.writeFM64,
				mockParameter("filename", $string),
				mockParameter("matrix", $matrixFloat64)
		);
		
		addMockFunction(runtimeFunctions, "writeFloatMatrix16", $void, Primitive.writeFM16,
				mockParameter("filename", $string),
				mockParameter("matrix", $matrixFloat16)
		);
		
		addMockFunction(runtimeFunctions, "writeFloatMatrix9", $void, Primitive.writeFM9,
				mockParameter("filename", $string),
				mockParameter("matrix", $matrixFloat9)
		);
		
		/*
		 * Math
		 */
		addMockFunction(runtimeFunctions, "powInt", $int, Primitive.powInt,
				mockParameter("base", $int),
				mockParameter("exp", $int)
		);
		
		addMockFunction(runtimeFunctions, "powFloat", $float, Primitive.powFloat,
				mockParameter("base", $float),
				mockParameter("exp", $float)
		);
		
		addMockFunction(runtimeFunctions, "sqrtInt", $int, Primitive.sqrtInt,
				mockParameter("num", $int)
		);
		
		addMockFunction(runtimeFunctions, "sqrtFloat", $float, Primitive.sqrtFloat,
				mockParameter("num", $float)
		);
		
		addMockFunction(runtimeFunctions, "modulo", $int, Primitive.modI,
				mockParameter("num", $int),
				mockParameter("divisor", $int)
		);
		
		/*
		 * Input & Output
		 */
		addMockFunction(runtimeFunctions, "printInt", $void, Primitive.printInt,
				mockParameter("num", $int)
		);
		
		addMockFunction(runtimeFunctions, "printFloat", $void, Primitive.printFloat,
				mockParameter("num", $float)
		);
		
		addMockFunction(runtimeFunctions, "printBool", $void, Primitive.printBool,
				mockParameter("b", $bool)
		);
		
		addMockFunction(runtimeFunctions, "printString", $void, Primitive.printString,
				mockParameter("text", $string)
		);
		
		addMockFunction(runtimeFunctions, "printLine", $void, Primitive.printLine);
		
		addMockFunction(runtimeFunctions, "readInt", $int, Primitive.readInt);
		
		addMockFunction(runtimeFunctions, "readFloat", $float, Primitive.readFloat);
		
		addMockFunction(runtimeFunctions, "readBool", $bool, Primitive.readBool);
		
		
		/*
		 * Type conversions
		 */
		addMockFunction(runtimeFunctions, "float2int", $int, Primitive.float2int,
				mockParameter("num", $float)
		);
		
		addMockFunction(runtimeFunctions, "int2float", $float, Primitive.int2float,
				mockParameter("num", $int)
		);
		
		/*
		 * Other
		 */
		addMockFunction(runtimeFunctions, "error", $float, Primitive.err,
				mockParameter("message", $string)
		);
		
		return runtimeFunctions;
	}
	
	private static FormalParameter mockParameter(String name, Type type) {
		FormalParameter parameter = new FormalParameter(SourceLocation.unknown, name, null);
		parameter.setType(type);
		return parameter;
	}
	
	@SuppressWarnings("unchecked")
	private static Function mockFunction(String name, Type returnType, Primitive primitive, FormalParameter... parameters) {
		Function function = new Function(SourceLocation.unknown, name, null, new ArrayList<>(Arrays.asList(parameters)), null);
		function.setReturnType(returnType);
		function.setCodeBaseOffset(Primitive.baseAddress + primitive.displacement);
		return function;
	}
	
	private static void addMockFunction(Map<String, Function> runtimeFunctions, String name, Type returnType, Primitive primitive, FormalParameter... parameters) {
		runtimeFunctions.put(name, mockFunction(name, returnType, primitive, parameters));
	}
	
	// This enum only exists in p2 because the actual Primitive class is not included in the p2 build.
	public enum Primitive {
		err,
		
		modI,
		
		readImage,
		writeImage,
		
		readIM64,
		readIM16,
		readIM9,
		writeIM64,
		writeIM16,
		writeIM9,
		
		readFM64,
		readFM16,
		readFM9,
		writeFM64,
		writeFM16,
		writeFM9,
		
		powInt,
		powFloat,
		sqrtInt,
		sqrtFloat,
		
		printInt,
		printFloat,
		printBool,
		printString,
		printLine,
		
		readInt,
		readFloat,
		readBool,
		
		int2float,
		float2int;
		
		public final int displacement = 0;
		public static final int baseAddress = 0;
	}
}
