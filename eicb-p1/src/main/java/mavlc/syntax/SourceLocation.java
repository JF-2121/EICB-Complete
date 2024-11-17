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
package mavlc.syntax;

import java.util.Objects;

public final class SourceLocation {
	
	public final int line;
	public final int column;
	
	public static final SourceLocation unknown = new SourceLocation(-1, -1);
	
	public SourceLocation(int line, int column) {
		this.line = line;
		this.column = column;
	}
	
	@Override
	public String toString() {
		if(line == -1 || column == -1) return "unknown location";
		return String.format("line %d, column %d", line, column);
	}
	
	@Override
	public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		SourceLocation that = (SourceLocation) o;
		return line == that.line && column == that.column;
	}
	
	@Override
	public int hashCode() {
		return Objects.hash(line, column);
	}
}
