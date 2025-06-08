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
package mavlc.util;

public class TextUtil {
	private TextUtil() { }
	
	public static String padRight(String str, int len) {
		return padRight(str, len, ' ');
	}
	
	public static String padRight(String str, int len, char ch) {
		if(str.length() >= len) return str.substring(0, len);
		return str + new String(new char[len - str.length()]).replace('\0', ch);
	}
	
	public static String padLeft(String str, int len) {
		return padLeft(str, len, ' ');
	}
	
	public static String padLeft(String str, int len, char ch) {
		if(str.length() >= len) return str.substring(str.length() - len);
		return new String(new char[len - str.length()]).replace('\0', ch) + str;
	}
}
