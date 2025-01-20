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
package mtam.errors;

import java.util.Optional;

public class ExecutionException extends RuntimeException {
	private static final long serialVersionUID = 3489430766335294709L;
	
	public final ErrorCode errorCode;
	private Optional<Integer> errorLocation;

	public ExecutionException(ErrorCode code, String message) {
		super(message);
		errorCode = code;
		errorLocation = Optional.empty();
	}

	public ExecutionException(ErrorCode code, String message, int location) {
		super(message);
		errorCode = code;
		errorLocation = Optional.of(location);
	}

	public Optional<Integer> getErrorLocation() {
		return errorLocation;
	}

	public void setErrorLocation(int errorLocation) {
		this.errorLocation = Optional.of(errorLocation);
	}
}
