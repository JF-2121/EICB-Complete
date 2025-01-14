/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 *
 * All rights reserved.
 *
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.errors;

public class FormattingHelpers {
	private FormattingHelpers() { }
	
	public static String highlightingPrefix = "> ";
	
	public static String concat(Object... parts) {
		StringBuilder sb = new StringBuilder();
		for(Object part : parts) sb.append(part);
		return sb.toString();
	}
	
	public static String highlight(String content) {
		return highlightingPrefix + content.replace("\n", '\n' + highlightingPrefix);
	}
}
