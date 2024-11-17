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

import mavlc.util.Ansi;
import org.apache.commons.cli.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * The MAVL compiler driver.
 */
public class Driver {
	
	public static boolean verbose;
	
	public static void main(String[] args) {
		try {
			Options options = setupOptions();
			CommandLine cmd = new DefaultParser().parse(options, args);
			boolean dumpErrors = cmd.hasOption("dump-errors");
			Path errorPath = dumpErrors ? Paths.get(cmd.getOptionValue("dump-errors")) : null;
			
			verbose = !cmd.hasOption("quiet");
			if(!cmd.hasOption("color") && System.getProperty("os.name").toLowerCase().contains("win") && System.getenv("MAVL_COLOR") == null) {
				Ansi.disable();
				// System.out.println("================================================================");
				// System.out.println("       WARNING: Color output not supported under windows.");
				// System.out.println(" To enable it anyway (e.g. in IntelliJ), define the environment");
				// System.out.println("        variable MAVL_COLOR or pass the argument --color.");
				// System.out.println("================================================================");
				// System.out.println();
			} else {
				Ansi.enable();
			}
			
			if(cmd.hasOption("help")) {
				printHelp(options);
				System.exit(0);
			}
			
			if(cmd.getArgList().isEmpty()) {
				System.err.println(Ansi.red + "No input file specified." + Ansi.reset);
				System.exit(1);
			}
			
			Path sourcePath = Paths.get(cmd.getArgList().get(0));
			if(!Files.exists(sourcePath) || !Files.isReadable(sourcePath)) {
				System.err.println(Ansi.red + "The specified input file " + sourcePath.toAbsolutePath() + " is not accessible!" + Ansi.reset);
				System.exit(1);
			}
			
			Set<Task> tasks = parseTasks(cmd);
			
			if(tasks.isEmpty()) {
				Task task = defaultTask();
				if(verbose) System.err.println(Ansi.yellow + "No tasks selected, defaulting to " + task + "." + Ansi.reset);
				tasks.add(task);
			}
			
			Task.resolveDependencies(tasks);
			
			Pipeline pipeline = new Pipeline();
			
			////////////////////////////////////////
			//////// execute selected tasks ////////
			////////////////////////////////////////
			
			if(tasks.contains(Task.parse)) {
				logTask(Task.parse);
				if(!pipeline.parseProgram(sourcePath))
					handleError(pipeline, errorPath);
			}
			
			if(tasks.contains(Task.dumpSource)) {
				logTask(Task.dumpSource);
				pipeline.dumpSource(getDumpPath(cmd, "dump-source", sourcePath, "_dump.mavl"));
			}
			
			if(tasks.contains(Task.dumpHtml)) {
				logTask(Task.dumpHtml);
				pipeline.dumpHtml(getDumpPath(cmd, "dump-html", sourcePath, "_dump.html"));
			}
			
			if(tasks.contains(Task.dumpXmlAst)) {
				logTask(Task.dumpXmlAst);
				pipeline.dumpAstXml(getDumpPath(cmd, "dump-xml-ast", sourcePath, ".syn.xml"));
			}
			
			if(tasks.contains(Task.dumpDotAst)) {
				logTask(Task.dumpDotAst);
				pipeline.dumpDot(getDumpPath(cmd, "dump-dot-ast", sourcePath, ".syn.dot"), false);
			}
			
			if(tasks.contains(Task.analyze)) {
				logTask(Task.analyze);
				if(!pipeline.analyzeProgram())
					handleError(pipeline, errorPath);
			}
			
			if(tasks.contains(Task.dumpXmlDast)) {
				logTask(Task.dumpXmlDast);
				pipeline.dumpDastXml(getDumpPath(cmd, "dump-xml-dast", sourcePath, ".ctx.xml"));
			}
			
			if(tasks.contains(Task.dumpDotDast)) {
				logTask(Task.dumpDotDast);
				pipeline.dumpDot(getDumpPath(cmd, "dump-dot-dast", sourcePath, ".ctx.dot"), true);
			}
			
			if(tasks.contains(Task.compile)) {
				logTask(Task.compile);
				if(!pipeline.compileProgram())
					handleError(pipeline, errorPath);
			}
			
			if(tasks.contains(Task.dumpImage)) {
				logTask(Task.dumpImage);
				pipeline.dumpImage(getDumpPath(cmd, "dump-image", sourcePath, ".tam"));
			}
			
			if(tasks.contains(Task.dumpSymbols)) {
				logTask(Task.dumpSymbols);
				pipeline.dumpSymbols(getDumpPath(cmd, "dump-symbols", sourcePath, ".sym"));
			}
			
			if(tasks.contains(Task.dumpDisasm)) {
				logTask(Task.dumpDisasm);
				pipeline.dumpDisasm(getDumpPath(cmd, "dump-disasm", sourcePath, "_disasm.txt"));
			}
			
			if(tasks.contains(Task.execute)) {
				logTask(Task.execute);
				if(!pipeline.executeProgram())
					handleError(pipeline, errorPath);
			}
			
			if(tasks.contains(Task.dumpOutput)) {
				logTask(Task.dumpOutput);
				pipeline.dumpOutput(getDumpPath(cmd, "dump-output", sourcePath, ".txt"));
			}
			
			if(tasks.contains(Task.dumpProfile)) {
				logTask(Task.dumpProfile);
				pipeline.dumpProfile(getDumpPath(cmd, "dump-profile", sourcePath, "_profile.txt"));
			}
			
		} catch(ParseException e) {
			e.printStackTrace();
			System.exit(1);
		}
	}
	
	private static Path getDumpPath(CommandLine cmd, String option, Path sourcePath, String extension) {
		String[] values = cmd.getOptionValues(option);
		if(values != null && values.length == 1) return sourcePath.toAbsolutePath().getParent().resolve(values[0]);
		String name = sourcePath.getFileName().toString();
		return sourcePath.resolveSibling(name.substring(0, name.lastIndexOf('.')) + extension);
	}
	
	private static void handleError(Pipeline pipeline, Path dumpPath) {
		if(dumpPath == null)
			pipeline.throwError();
		
		Exception error = pipeline.getError();
		if(error == null)
			return;
		
		try {
			assert dumpPath != null;
			Files.write(dumpPath, pipeline.getError().getMessage().getBytes());
		} catch(IOException e) {
			e.printStackTrace();
		}
	}
	
	private static void logTask(Task task) {
		if(verbose) System.out.println("- Running task: " + Ansi.brightMagenta + task + Ansi.reset);
	}
	
	private static void printHelp(Options options) {
		List<Option> general = new ArrayList<>(options.getOptions());
		
		System.out.println("Basic usage: ");
		System.out.println(Ansi.white + "  mavlc " + Ansi.brightGreen + "[stages]" + Ansi.reset + " " + Ansi.brightRed + "<sourcePath>" + Ansi.reset + " " + Ansi.brightCyan + "[options]" + Ansi.reset);
		
		System.out.println();
		System.out.println("Available stages");
		System.out.println(Ansi.white + "Each stage depends on the previous one." + Ansi.reset);
		System.out.println(Ansi.white + "If the previous stage is not selected, it will be enabled automatically." + Ansi.reset);
		for(TaskCollection.Entry entry : stageOptions) {
			Option option = entry.option;
			printOption(option, Ansi.brightGreen);
			general.remove(option);
		}
		
		System.out.println();
		System.out.println("Dump options");
		System.out.println(Ansi.white + "The following options allow to output the results of each stage in various formats." + Ansi.reset);
		System.out.println(Ansi.white + "If no stage or dump task is selected, " + defaultTask() + " is selected by default.");
		for(TaskCollection.Entry entry : dumpOptions) {
			Task task = entry.task;
			Option option = entry.option;
			printOption(option, Ansi.brightCyan);
			System.out.println("    " + Ansi.white + option.getDescription().replace("\n", "\n    ") + Ansi.reset);
			if(task != null && task.prerequisite != null)
				System.out.println("    " + Ansi.white + "Depends on the " + Ansi.brightGreen + task.prerequisite + Ansi.white + " stage" + Ansi.reset);
			general.remove(option);
		}
		
		System.out.println();
		System.out.println("General options");
		for(Option option : general) {
			printOption(option, Ansi.brightCyan);
			System.out.println("    " + Ansi.white + option.getDescription() + Ansi.reset);
		}
	}
	
	public static Task defaultTask() {
		return Task.dumpDotAst;
	}
	
	private static void printOption(Option option, Ansi color) {
		System.out.print("  " + color + "-" + option.getOpt() + Ansi.reset);
		if(option.hasLongOpt()) {
			System.out.print(", " + color + "--" + option.getLongOpt() + Ansi.reset);
		}
		if(option.hasArg()) {
			System.out.print(" " + Ansi.brightRed + "[" + option.getArgName() + "]" + Ansi.reset);
		}
		System.out.println();
	}
	
	private static Set<Task> parseTasks(CommandLine cmd) {
		Set<Task> tasks = new HashSet<>();
		
		if(cmd.hasOption('p')) tasks.add(Task.parse);
		if(cmd.hasOption('a')) tasks.add(Task.analyze);
		if(cmd.hasOption('c')) tasks.add(Task.compile);
		if(cmd.hasOption('e')) tasks.add(Task.execute);
		
		if(cmd.hasOption("ds")) tasks.add(Task.dumpSource);
		if(cmd.hasOption("dh")) tasks.add(Task.dumpHtml);
		if(cmd.hasOption("dxa")) tasks.add(Task.dumpXmlAst);
		if(cmd.hasOption("dxd")) tasks.add(Task.dumpXmlDast);
		if(cmd.hasOption("dda")) tasks.add(Task.dumpDotAst);
		if(cmd.hasOption("ddd")) tasks.add(Task.dumpDotDast);
		if(cmd.hasOption("di")) tasks.add(Task.dumpImage);
		if(cmd.hasOption("dd")) tasks.add(Task.dumpDisasm);
		if(cmd.hasOption("dds")) tasks.add(Task.dumpSymbols);
		if(cmd.hasOption("do")) tasks.add(Task.dumpOutput);
		if(cmd.hasOption("dp")) tasks.add(Task.dumpProfile);
		
		return tasks;
	}
	
	private static final TaskCollection stageOptions = new TaskCollection();
	private static final TaskCollection dumpOptions = new TaskCollection();
	
	private static Options setupOptions() {
		Options options = new Options();
		
		options.addOption("h", "help", false, "Print this help text");
		options.addOption("q", "quiet", false, "Less verbose output");
		options.addOption("col", "color", false, "Enable color output");
		
		options.addOption(buildStageOption("p", "parse", Task.parse));
		options.addOption(buildStageOption("a", "analyze", Task.analyze));
		options.addOption(buildStageOption("c", "compile", Task.compile));
		options.addOption(buildStageOption("e", "execute", Task.execute));
		
		options.addOption(buildDumpOption("ds", "dump-source", "Dumps the formatted mavl source code", Task.dumpSource));
		options.addOption(buildDumpOption("dh", "dump-html", "Dumps the source code as highlighted html", Task.dumpHtml));
		options.addOption(buildDumpOption("dxa", "dump-xml-ast", "Dumps the ast as xml", Task.dumpXmlAst));
		options.addOption(buildDumpOption("dxd", "dump-xml-dast", "Dumps the dast as xml", Task.dumpXmlDast));
		options.addOption(buildDumpOption("dda", "dump-dot-ast", "Dumps the ast as dot graph", Task.dumpDotAst));
		options.addOption(buildDumpOption("ddd", "dump-dot-dast", "Dumps the dast as dot graph", Task.dumpDotDast));
		options.addOption(buildDumpOption("di", "dump-image", "Dumps the program image", Task.dumpImage));
		options.addOption(buildDumpOption("dd", "dump-disasm", "Dumps the program disassembly", Task.dumpDisasm));
		options.addOption(buildDumpOption("dds", "dump-symbols", "Dumps debug symbols", Task.dumpSymbols));
		options.addOption(buildDumpOption("do", "dump-output", "Dumps the execution output", Task.dumpOutput));
		options.addOption(buildDumpOption("dp", "dump-profile", "Dumps the execution profile", Task.dumpProfile));
		options.addOption(buildDumpOption("de", "dump-errors", "Dumps any errors thrown by one of the pipeline stages", null));
		
		return options;
	}
	
	private static Option buildStageOption(String shortForm, String longForm, Task task) {
		Option option = Option.builder(shortForm)
				.longOpt(longForm)
				.build();
		stageOptions.add(task, option);
		return option;
	}
	
	private static Option buildDumpOption(String shortForm, String longForm, String description, Task task) {
		Option option = Option.builder(shortForm)
				.longOpt(longForm)
				.desc(description)
				.hasArg(true)
				.argName("path")
				.optionalArg(true)
				.numberOfArgs(1)
				.build();
		dumpOptions.add(task, option);
		return option;
	}
	
	private static class TaskCollection extends ArrayList<TaskCollection.Entry> {
		private static final long serialVersionUID = 3788086025777767107L;
		
		public void add(Task task, Option option) {
			add(new Entry(task, option));
		}
		
		public static class Entry {
			public final Task task;
			public final Option option;
			
			public Entry(Task task, Option option) {
				this.task = task;
				this.option = option;
			}
		}
	}
}
