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
package mavlc.errors;

public class MissingMainFunctionError extends CompilationError {
	
	private static final long serialVersionUID = 5999673903216026599L;
	
	public MissingMainFunctionError() {
		this.message = "The module does not contain a function with the signature \"function void main()\".";
	}
}
