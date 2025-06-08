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

import java.util.ArrayList;
import java.util.Set;

public enum Task {
	parse(null),
	dumpSource(parse),
	dumpHtml(parse),
	dumpXmlAst(parse),
	dumpDotAst(parse),
	
	analyze(parse),
	dumpXmlDast(analyze),
	dumpDotDast(analyze),
	
	compile(analyze),
	dumpImage(compile),
	dumpDisasm(compile),
	dumpSymbols(compile),
	
	execute(compile),
	dumpOutput(execute),
	dumpProfile(execute);
	
	public final Task prerequisite;
	
	Task(Task prerequisite) {
		this.prerequisite = prerequisite;
	}
	
	public static void resolveDependencies(Set<Task> tasks) {
		for(Task task : new ArrayList<>(tasks)) {
			while(task.prerequisite != null && tasks.add(task.prerequisite)) {
				if(Driver.verbose) System.out.println("Task " + Ansi.brightMagenta + task + Ansi.reset + " depends on task " + Ansi.brightMagenta + task.prerequisite + Ansi.reset + ".");
				task = task.prerequisite;
			}
		}
	}
}
