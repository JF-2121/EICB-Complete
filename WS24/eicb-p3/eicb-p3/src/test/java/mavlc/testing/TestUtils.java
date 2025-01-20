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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TestUtils {
	private TestUtils() { }
	
	public static void flush() {
		try {
			// flush and wait for a few milliseconds
			// without this, IntelliJ sometimes prints the line to the wrong test case output
			System.err.flush();
			System.out.flush();
			Thread.sleep(3);
		} catch(InterruptedException ignored) {
		}
	}
	
	public static Collection<Object[]> findTestCases(Path testCasesRoot) {
		return findSourceFiles(testCasesRoot).map(testCase -> {
			String localPath = testCasesRoot.relativize(testCase).getParent().toString();
			String fileName = testCase.getFileName().toString();
			fileName = fileName.substring(0, fileName.indexOf('.'));
			
			String name = Paths.get(localPath, fileName).toString().replace('\\', '/');
			
			// first parameter: path to the test case xml file
			// second parameter: test name
			return new Object[]{testCase, name};
		}).collect(Collectors.toList());
	}
	
	public static Stream<Path> findSourceFiles(Path rootDirectory) {
		try {
			return Files.find(rootDirectory, Integer.MAX_VALUE, (p, a) -> a.isRegularFile() && p.toString().endsWith(".mavl"));
		} catch(IOException e) {
			e.printStackTrace();
		}
		return Stream.empty();
	}
}
