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

public enum Ansi {
	ESC("\033"),
	CSI("\033["),
	
	reset("\033[0m"),
	invert("\033[7m"),
	resetColor("\033[39m"),
	
	black("\033[30m"),
	red("\033[31m"),
	green("\033[32m"),
	yellow("\033[33m"),
	blue("\033[34m"),
	magenta("\033[35m"),
	cyan("\033[36m"),
	white("\033[37m"),
	
	brightBlack("\033[90m"),
	brightRed("\033[91m"),
	brightGreen("\033[92m"),
	brightYellow("\033[93m"),
	brightBlue("\033[94m"),
	brightMagenta("\033[95m"),
	brightCyan("\033[96m"),
	brightWhite("\033[97m"),
	
	blackBackground("\033[40m"),
	redBackground("\033[41m"),
	greenBackground("\033[42m"),
	yellowBackground("\033[43m"),
	blueBackground("\033[44m"),
	magentaBackground("\033[45m"),
	cyanBackground("\033[46m"),
	whiteBackground("\033[47m"),
	
	brightBlackBackground("\033[100m"),
	brightRedBackground("\033[101m"),
	brightGreenBackground("\033[102m"),
	brightYellowBackground("\033[103m"),
	brightBlueBackground("\033[104m"),
	brightMagentaBackground("\033[105m"),
	brightCyanBackground("\033[106m"),
	brightWhiteBackground("\033[107m");
	
	public static boolean isEnabled;
	
	private final String code;
	
	Ansi(String code) {
		this.code = code;
	}
	
	@Override public String toString() {
		return isEnabled ? code : "";
	}
	
	public static void enable() {
		isEnabled = true;
	}
	
	public static void disable() {
		isEnabled = false;
	}
}
