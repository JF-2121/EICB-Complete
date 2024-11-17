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
package mavlc.testing;

import mavlc.Pipeline;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Objects;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

@RunWith(Parameterized.class)
public class ErrorCompareTest extends BaseCompareTest {
	
	public ErrorCompareTest(Path srcFilePath, String testName) {
		super(srcFilePath, testName, ".txt", ".mavl");
	}
	
	@Override
	public void run() {
		// load reference data
		boolean onlySyntax = refFilePath.toString().endsWith(".syn.txt");
		
		Pipeline pipeline = new Pipeline();
		pipeline.parseProgram(srcFilePath);
		if(!onlySyntax) {
			pipeline.throwError();
			pipeline.analyzeProgram();
		}
		Exception error = pipeline.getError();
		
		assertNotNull("Expected an error but got none!", error);
		try {
			String refMessage = new String(Files.readAllBytes(refFilePath)).replace("\r", "");
			String outMessage = error.getMessage();
			if(!Objects.equals(refMessage, outMessage)) {
				StringBuilder sb = new StringBuilder();
				sb.append("Error thrown by the compiler does not match the expectation!\n");
				sb.append("\nExpected error: \n  ");
				sb.append(refMessage.replace("\n", "\n  "));
				sb.append("\n\nError thrown by compiler:\n  ");
				sb.append(error.getMessage().replace("\n", "\n  "));
				sb.append("\n\nOriginal stack trace:\n");
				for(StackTraceElement frame : error.getStackTrace()) {
					if(frame.getClassName().startsWith("sun.reflect")) break;
					if(frame.getClassName().startsWith("org.junit")) break;
					sb.append("  ").append(frame).append("\n");
				}
				fail(sb.toString());
			}
		} catch(IOException e) {
			e.printStackTrace();
			fail("Failed to load reference error");
		}
	}
	
	@Parameters(name = "{1}")
	public static Collection<Object[]> data() {
		return TestUtils.findTestCases(Paths.get("src", "test", "testcases", "errors"));
	}
}
