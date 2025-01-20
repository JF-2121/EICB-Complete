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

import mavlc.syntax.AstNode;

import java.util.List;
import java.util.Map;

public class VisualGraph {
	public final VisualElement.VisualNode rootNode;
	public final List<VisualElement.VisualNode> visualNodes;
	public final Map<AstNode, VisualElement.VisualNode> astToVisual;
	
	public VisualGraph(VisualElement.VisualNode root, List<VisualElement.VisualNode> visualNodes, Map<AstNode, VisualElement.VisualNode> astToVisual) {
		this.rootNode = root;
		this.visualNodes = visualNodes;
		this.astToVisual = astToVisual;
	}
}
