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

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.Timeout;

import java.nio.file.Path;

public abstract class BaseCompareTest {
	
	@Rule
	public Timeout timeout = Timeout.millis(0);
	
	protected final Path refDirectory;
	protected final Path srcDirectory;
	
	protected final Path refFilePath;
	protected final Path srcFilePath;
	
	protected final String baseName;
	protected final String testName;
	
	public BaseCompareTest(Path srcFilePath, String testName, String refFileExtension, String srcFileExtension) {
		String srcFileName = srcFilePath.getFileName().toString();
		
		this.baseName = srcFileName.substring(0, srcFileName.lastIndexOf('.'));
		this.testName = testName;
		
		this.refDirectory = srcFilePath.getParent();
		this.srcDirectory = srcFilePath.getParent();
		
		this.refFilePath = refDirectory.resolve(baseName + refFileExtension);
		this.srcFilePath = srcDirectory.resolve(baseName + srcFileExtension);
		
		String envTimeout = System.getenv("MAVL_TIMEOUT");
		if(envTimeout != null) timeout = Timeout.millis(Integer.parseInt(envTimeout));
	}
	
	@Test
	public void test() {
		System.out.println("=== Running test case " + testName + " ===");
		System.out.println();
		TestUtils.flush();
		
		run();
		
		System.out.println();
		System.out.println("=== Test case " + testName + " finished successfully ===");
		System.out.println();
		TestUtils.flush();
	}
	
	protected abstract void run();
}
