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
import mavlc.services.serialization.XmlSerialization;
import mavlc.syntax.module.Module;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;

@RunWith(Parameterized.class)
public class AstCompareTest extends BaseCompareTest {
	
	public AstCompareTest(Path srcFilePath, String testName) {
		super(srcFilePath, testName, ".xml", ".mavl");
	}
	
	@Override
	public void run() {
		boolean onlySyntax = refFilePath.toString().endsWith(".syn.xml");
		Module refModule = XmlSerialization.deserialize(refFilePath.toString());
		
		Pipeline pipeline = new Pipeline();
		if(!pipeline.parseProgram(srcFilePath)) pipeline.throwError();
		if(!onlySyntax && !pipeline.analyzeProgram()) pipeline.throwError();

		// Let the ASTComparator compare the produced AST against the XML-serialized reference.
		new AstComparator().compare(pipeline.getAst(), refModule);
	}
	
	@Parameters(name = "{1}")
	public static Collection<Object[]> data() {
		return TestUtils.findTestCases(Paths.get("src", "test", "testcases", "trees"));
	}
}
