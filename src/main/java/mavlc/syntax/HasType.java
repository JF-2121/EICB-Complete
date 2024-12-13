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

import mavlc.type.Type;

/**
 * Superinterface for everything with an associated type.
 */
public interface HasType {

	/**
	 * Reports whether a type is currently available on this node.
	 */
	boolean isTypeSet();

	/**
	 * Returns the type currently associated with this node.
	 *
	 * @return The type associated with this node. This method never returns null.
	 * @throws mavlc.errors.InternalCompilerError If this method is called in a state where {@link #isTypeSet()}
	 * returned {@code false}.
	 * */
	Type getType();

	/**
	 * Sets the type of this node.
	 *
	 * @param type the type to set. Some nodes, such as literal values or simple type specifiers, may restrict the types
	 *             supported.
	 */
	void setType(Type type);
}
