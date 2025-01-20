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
package mavlc.services.visualization;

import java.io.PrintWriter;

public interface GraphBuilder {
	/**
	 * Exports a {@link VisualGraph}.
	 *
	 * @param graph The {@link VisualGraph} to be exported
	 * @param out A {@link PrintWriter} to write the output to
	 * @param decorate Whether to print references (requires the graph to be generated with decorate = true as well)
	 */
	void buildGraph(VisualGraph graph, PrintWriter out, boolean decorate);
}
