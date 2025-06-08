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

import static org.junit.Assert.fail;

@RunWith(Parameterized.class)
public class OutputCompareTest extends BaseCompareTest {
	
	public OutputCompareTest(Path srcFilePath, String testName) {
		super(srcFilePath, testName, ".txt", ".mavl");
	}
	
	private static final int maxCycles = 100000;
	
	@Override
	public void run() {
		
		Pipeline pipeline = new Pipeline();
		if(!pipeline.parseProgram(srcFilePath)) pipeline.throwError();
		if(!pipeline.analyzeProgram()) pipeline.throwError();
		if(!pipeline.compileProgram()) pipeline.throwError();
		if(!pipeline.executeProgram(maxCycles)) pipeline.throwError();
		
		try {
			String refOutput = new String(Files.readAllBytes(refFilePath)).replace("\r", "");
			String output = pipeline.getOutput().replace("\r", "");
			
			if(output.equals(refOutput)) {
				System.out.println("Output:\n" + output);
			} else {
				fail("\nResulting output does not match reference:\n" +
						"Reference:\n" + refOutput + "\n" +
						"Output:\n" + output + "\n");
			}
		} catch(IOException e) {
			throw new RuntimeException(e);
		}
	}
	
	@Parameters(name = "{1}")
	public static Collection<Object[]> data() {
		return TestUtils.findTestCases(Paths.get("src", "test", "testcases", "executables"));
	}
}
