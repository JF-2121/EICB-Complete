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
package mavlc;

import mavlc.errors.InternalCompilerError;
import mavlc.parsing.Parser;
import mavlc.parsing.Scanner;
import mavlc.services.serialization.XmlSerialization;
import mavlc.services.visualization.Dumper;
import mavlc.services.visualization.HtmlDumper;
import mavlc.services.visualization.VisualGraph;
import mavlc.services.visualization.Visualizer;
import mavlc.services.visualization.dot.DotBuilder;
import mavlc.syntax.AstNode;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.output.TeeOutputStream;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.security.*;
import java.security.cert.Certificate;

@SuppressWarnings({"UnusedReturnValue", "BooleanMethodIsAlwaysInverted"})
public class Pipeline {
	protected Stage stage = Stage.uninitialized;
	
	protected Path sourcePath;
	protected AstNode ast;
	protected String output;
	protected Exception error;
	
	protected final AccessControlContext sandbox;
	
	public Stage currentStage() {
		return stage;
	}
	
	public AstNode getAst() {
		return ast;
	}
	
	public String getOutput() {
		return output;
	}
	
	public Exception getError() {
		return error;
	}
	
	public RuntimeException getRuntimeError() {
		return error instanceof RuntimeException ? (RuntimeException) error : new RuntimeException(error);
	}
	
	public void throwError() {
		if(error != null) throw getRuntimeError();
	}
	
	public Pipeline() {
		Permissions perms = new Permissions(); /* empty, i.e. no permissions will be granted */
		CodeSource source = new CodeSource(null /* all locations */, (Certificate[]) null /* unsigned code */);
		ProtectionDomain[] domains = new ProtectionDomain[]{new ProtectionDomain(source, perms)};
		sandbox = new AccessControlContext(domains);
	}
	
	/**
	 * Parses a program.<br>
	 * <br>
	 * Requires (stage == Stage.uninitialized)<br>
	 * Ensures (stage == Stage.syntax)<br>
	 *
	 * @param sourcePath Path to the source file
	 * @return Whether the stage completed successfully
	 */
	public boolean parseProgram(Path sourcePath) {
		if(stage != Stage.uninitialized)
			throw new InternalCompilerError("Cannot parse program: A program has already been parsed in this pipeline");
		this.sourcePath = sourcePath;
		try {
			parseProgramImpl(sourcePath);
			stage = Stage.syntax;
			return true;
		} catch(Exception e) {
			error = e;
			return false;
		}
	}
	
	/**
	 * Analyzes a parsed program.<br>
	 * <br>
	 * Requires (stage == Stage.syntax)<br>
	 * Ensures (stage == Stage.context)<br>
	 *
	 * @return Whether the stage completed successfully
	 */
	public boolean analyzeProgram() {
		if(stage.value < Stage.syntax.value)
			throw new InternalCompilerError("Cannot analyze program: No program has been parsed yet");
		if(stage.value > Stage.syntax.value)
			throw new InternalCompilerError("Cannot analyze program: A program has already been analyzed in this pipeline");
		try {
			analyzeProgramImpl();
			stage = Stage.context;
			return true;
		} catch(Exception e) {
			error = e;
			return false;
		}
	}
	
	/**
	 * Compiles an analyzed program to a sequence of instructions.<br>
	 * <br>
	 * Requires (stage == Stage.context)<br>
	 * Ensures (stage == Stage.codegen)<br>
	 *
	 * @return Whether the stage completed successfully
	 */
	public boolean compileProgram() {
		if(stage.value < Stage.context.value)
			throw new InternalCompilerError("Cannot compile program: No program has been analyzed yet");
		if(stage.value > Stage.context.value)
			throw new InternalCompilerError("Cannot compile program: A program has already been compiled in this pipeline");
		try {
			compileProgramImpl();
			stage = Stage.codegen;
			return true;
		} catch(Exception e) {
			error = e;
			return false;
		}
	}
	
	/**
	 * Executes a compiled program using the mtam interpreter.<br>
	 * <br>
	 * Requires (stage == Stage.codegen)<br>
	 * Ensures (stage == Stage.execute)<br>
	 *
	 * @return Whether the stage completed successfully
	 */
	public boolean executeProgram() {
		return executeProgram(0);
	}
	
	/**
	 * Executes a compiled program using the mtam interpreter.<br>
	 * <br>
	 * Requires (stage == Stage.codegen)<br>
	 * Ensures (stage == Stage.execute)<br>
	 *
	 * @param maxCycles The maximum number of cycles to simulate
	 * @return Whether the stage completed successfully
	 */
	public boolean executeProgram(int maxCycles) {
		if(stage.value < Stage.codegen.value)
			throw new InternalCompilerError("Cannot execute program: No program has been compiled yet");
		if(stage.value > Stage.codegen.value)
			throw new InternalCompilerError("Cannot execute program: A program has already been executed in this pipeline");
		try {
			ByteArrayOutputStream buf = new ByteArrayOutputStream();
			OutputStream out = new TeeOutputStream(System.out, buf);
			executeProgramImpl(new ByteArrayInputStream(new byte[0]), out, maxCycles);
			output = buf.toString(StandardCharsets.UTF_8);
			stage = Stage.execute;
			return true;
		} catch(Exception e) {
			error = e;
			return false;
		}
	}
	
	/**
	 * Executes a compiled program using the mtam interpreter.<br>
	 * <br>
	 * Requires (stage == Stage.codegen)<br>
	 * Ensures (stage == Stage.execute)<br>
	 *
	 * @param in The input stream
	 * @param out The output stream
	 * @param maxCycles The maximum number of cycles to simulate
	 * @return Whether the stage completed successfully
	 */
	public boolean executeProgram(InputStream in, OutputStream out, int maxCycles) {
		if(stage.value < Stage.codegen.value)
			throw new InternalCompilerError("Cannot execute program: No program has been compiled yet");
		if(stage.value > Stage.codegen.value)
			throw new InternalCompilerError("Cannot execute program: A program has already been executed in this pipeline");
		try {
			executeProgramImpl(in, out, maxCycles);
			output = "<output redirected>";
			stage = Stage.execute;
			return true;
		} catch(Exception e) {
			error = e;
			return false;
		}
	}
	
	protected void parseProgramImpl(Path sourcePath) throws IOException {
		Scanner scanner = new Scanner(sourcePath.toFile());
		AccessController.doPrivileged((PrivilegedAction<Void>) () -> {
			Parser parser = new Parser(scanner.scan());
			ast = parser.parse();
			return null;
		}, sandbox);
	}
	
	protected void analyzeProgramImpl() {
		throw new InternalCompilerError("Context analysis is not available in this build");
	}
	
	protected void compileProgramImpl() {
		throw new InternalCompilerError("Code generation is not available in this build");
	}
	
	protected void executeProgramImpl(InputStream in, OutputStream out, int maxCycles) {
		throw new InternalCompilerError("Interpreter is not available in this build");
	}
	
	public String getSource() {
		if(stage.value < Stage.syntax.value)
			throw new InternalCompilerError("Cannot dump source: No program has been parsed yet");
		return Dumper.dump(ast);
	}
	
	public boolean dumpSource(Path outPath) {
		try {
			FileUtils.write(outPath.toFile(), getSource());
			return true;
		} catch(IOException e) {
			error = e;
			return false;
		}
	}
	
	public String getHtml() {
		if(stage.value < Stage.syntax.value)
			throw new InternalCompilerError("Cannot dump source: No program has been parsed yet");
		return HtmlDumper.dump(ast);
	}
	
	public boolean dumpHtml(Path outPath) {
		try {
			FileUtils.write(outPath.toFile(), getHtml());
			return true;
		} catch(IOException e) {
			error = e;
			return false;
		}
	}
	
	public String getAstXml() {
		if(stage.value < Stage.syntax.value)
			throw new InternalCompilerError("Cannot dump AST: No program has been parsed yet");
		if(stage.value > Stage.syntax.value)
			throw new InternalCompilerError("Cannot dump undecorated AST: The program has already been analyzed");
		return XmlSerialization.serialize(ast);
	}
	
	public boolean dumpAstXml(Path outPath) {
		if(stage.value < Stage.syntax.value)
			throw new InternalCompilerError("Cannot dump AST: No program has been parsed yet");
		if(stage.value > Stage.syntax.value)
			throw new InternalCompilerError("Cannot dump undecorated AST: The program has already been analyzed");
		try {
			XmlSerialization.serialize(ast, outPath);
			return true;
		} catch(Exception e) {
			error = e;
			return false;
		}
	}
	
	public String getDastXml() {
		if(stage.value < Stage.context.value)
			throw new InternalCompilerError("Cannot dump DAST: No program has been analyzed yet");
		return XmlSerialization.serialize(ast);
	}
	
	public boolean dumpDastXml(Path outPath) {
		if(stage.value < Stage.context.value)
			throw new InternalCompilerError("Cannot dump DAST: No program has been analyzed yet");
		try {
			XmlSerialization.serialize(ast, outPath);
			return true;
		} catch(Exception e) {
			error = e;
			return false;
		}
	}
	
	public String getDot(boolean decorate) {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		dumpDot(out, decorate);
		return out.toString();
	}
	
	public boolean dumpDot(Path outPath, boolean decorate) {
		try {
			return dumpDot(new FileOutputStream(outPath.toFile()), decorate);
		} catch(FileNotFoundException e) {
			error = e;
			return false;
		}
	}
	
	public boolean dumpDot(OutputStream out, boolean decorate) {
		if(decorate && stage.value < Stage.context.value)
			throw new InternalCompilerError("Cannot dump DAST: No program has been analyzed yet");
		if(stage.value < Stage.syntax.value)
			throw new InternalCompilerError("Cannot dump AST: No program has been parsed yet");
		try {
			VisualGraph graph = new Visualizer().buildVisualGraph(ast, decorate);
			new DotBuilder().buildGraph(graph, new PrintWriter(out), decorate);
			return true;
		} catch(Exception e) {
			error = e;
			return false;
		}
	}
	
	public boolean dumpImage(Path outPath) {
		throw new InternalCompilerError("Code generation is not available in this build");
	}
	
	public boolean dumpImage(OutputStream out) {
		throw new InternalCompilerError("Code generation is not available in this build");
	}
	
	public boolean dumpSymbols(Path outPath) {
		throw new InternalCompilerError("Code generation is not available in this build");
	}
	
	public boolean dumpSymbols(OutputStream out) {
		throw new InternalCompilerError("Code generation is not available in this build");
	}
	
	
	public String getDisasm() {
		throw new InternalCompilerError("Code generation is not available in this build");
	}
	
	public boolean dumpDisasm(Path outPath) {
		throw new InternalCompilerError("Code generation is not available in this build");
	}
	
	public boolean dumpOutput(Path outPath) {
		throw new InternalCompilerError("Interpreter is not available in this build");
	}
	
	public boolean dumpOutput(OutputStream out) {
		throw new InternalCompilerError("Interpreter is not available in this build");
	}
	
	public boolean dumpProfile(Path outPath) {
		throw new InternalCompilerError("Interpreter is not available in this build");
	}
	
	public boolean dumpProfile(OutputStream out) {
		throw new InternalCompilerError("Interpreter is not available in this build");
	}
	
	private String getProfile() {
		throw new InternalCompilerError("Interpreter is not available in this build");
	}
	
	public enum Stage {
		uninitialized(0), syntax(1), context(2), codegen(3), execute(4);
		
		public final int value;
		
		Stage(int value) {
			this.value = value;
		}
	}
}
