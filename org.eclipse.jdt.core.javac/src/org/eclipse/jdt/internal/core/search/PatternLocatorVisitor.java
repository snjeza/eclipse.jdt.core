/*******************************************************************************
 * Copyright (c) 2023 Red Hat, Inc. and others.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *******************************************************************************/
package org.eclipse.jdt.internal.core.search;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.AnnotationTypeDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CreationReference;
import org.eclipse.jdt.core.dom.EnumConstantDeclaration;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.ExpressionMethodReference;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.IntersectionType;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NameQualifiedType;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.QualifiedType;
import org.eclipse.jdt.core.dom.RecordDeclaration;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SuperMethodReference;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.TypeParameter;
import org.eclipse.jdt.core.dom.UnionType;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.internal.core.search.matching.DOMPatternLocator;
import org.eclipse.jdt.internal.core.search.matching.MatchLocator;
import org.eclipse.jdt.internal.core.search.matching.NodeSetWrapper;
import org.eclipse.jdt.internal.core.search.matching.PatternLocator;

/**
 * Visits an AST to feel the possible match with nodes
 */
class PatternLocatorVisitor extends ASTVisitor {

	private final PatternLocator patternLocator;
	private final NodeSetWrapper nodeSet;
	private MatchLocator locator;
	private HashMap<PatternLocator, DOMPatternLocator> wrapperMap = new HashMap<>();

	public PatternLocatorVisitor(PatternLocator patternLocator, NodeSetWrapper nodeSet, MatchLocator locator) {
		super(true);
		this.patternLocator = patternLocator;
		this.nodeSet = nodeSet;
		this.locator = locator;
	}
	
	private DOMPatternLocator getWrapper(PatternLocator locator) {
		DOMPatternLocator l = wrapperMap.get(locator);
		if(l == null ) {
			l = DOMPatternLocatorFactory.createWrapper(locator);
			wrapperMap.put(locator, l);
		}
		return l;
	}

	private <T extends ASTNode> boolean defaultVisitImplementation(T node, BiFunction<T, DOMPatternLocator, LocatorResponse> levelFunc) {
		defaultVisitImplementationWithFunc(node, levelFunc, DOMASTNodeUtils::getBinding);
		return true;
	}

	/*
	 * TODO 
	 * We really need to change this BiFunction into something that returns
	 * a record with way more information.  Simply knowing if the current node matches
	 * is nowhere near sufficient. 
	 * 
	 * We need to know if: 
	 *   1) the current node matches or does not match
	 *   2) We found a replacement node to consider
	 *   3) We found a replacement node and added it already
	 *   4) Should we continue considering children or have they been ruled out?
	 */
	private <T extends ASTNode> LocatorResponse defaultVisitImplementationWithFunc(
			T node,
			BiFunction<T, DOMPatternLocator, LocatorResponse> levelFunc,
			Function<ASTNode, IBinding> bindingFunc) {
		boolean mustResolve = (this.nodeSet.getWrapped().mustResolve || this.patternLocator.isMustResolve());
		DOMPatternLocator wrapper = getWrapper(this.patternLocator);
		LocatorResponse resp = levelFunc.apply(node, wrapper);
		boolean nodeReplaced = resp.replacementNodeFound();
		ASTNode n2 = nodeReplaced ? resp.replacement() : node;
		n2 = n2 == null ? node : n2;
		if (resp.level() == PatternLocator.POSSIBLE_MATCH && mustResolve) {
			LocatorResponse resp2 = wrapper.resolveLevel(n2, bindingFunc.apply(n2), this.locator);
			resp = new LocatorResponse(resp2.level(), resp.replacementNodeFound(), n2, resp2.added(), resp2.canVisitChildren());
		}
		boolean added = resp.added();  
		if( !added ) {
			this.nodeSet.addMatch(n2, resp.level());
		}
		return resp;
	}


	@Override
	public boolean visit(AnnotationTypeDeclaration node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}
	@Override
	public boolean visit(TypeParameter node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}
	@Override
	public boolean visit(MethodDeclaration node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}
	@Override
	public boolean visit(MethodInvocation node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}
	@Override
	public boolean visit(ExpressionMethodReference node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}
	@Override
	public boolean visit(SuperMethodReference node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}
	@Override
	public boolean visit(SuperMethodInvocation node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}

	private boolean visitAbstractTypeDeclaration(AbstractTypeDeclaration node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}
	@Override
	public boolean visit(EnumDeclaration node) {
		return visitAbstractTypeDeclaration(node);
	}
	@Override
	public boolean visit(TypeDeclaration node) {
		return visitAbstractTypeDeclaration(node);
	}
	@Override
	public boolean visit(RecordDeclaration node) {
		return visitAbstractTypeDeclaration(node);
	}
	@Override
	public boolean visit(AnonymousClassDeclaration node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}

	private boolean visitType(Type node) {
		LocatorResponse resp = defaultVisitImplementationWithFunc(node, (x,y) -> y.match(node, this.nodeSet, this.locator), DOMASTNodeUtils::getBinding);
		return resp.level() == 0 && resp.canVisitChildren();
	}
	
	@Override
	public boolean visit(SimpleType type) {
		visitType(type);
		Name n = type.getName();
		if( n instanceof QualifiedName qn ) {
			Name qualifier = qn.getQualifier();
			if( qualifier instanceof SimpleName sn1 ) {
				sn1.accept(this);
			} else if( qualifier instanceof QualifiedName qn1) {
				qn1.accept(this);
			}
		}
		return false; // No need to visit single name child
	}
	@Override
	public boolean visit(QualifiedType type) {
		boolean ret = visitType(type);
		if( !ret ) {
			visitAllDescendentTypeArguments(type);
		}
		return ret;
	}
	@Override
	public boolean visit(NameQualifiedType type) {
		boolean ret = visitType(type);
		if( !ret ) {
			visitAllDescendentTypeArguments(type);
		}
		return ret;
	}
	@Override
	public boolean visit(ParameterizedType node) {
		LocatorResponse resp = defaultVisitImplementationWithFunc(node.getType(), (x,y) -> y.match(node, this.nodeSet, this.locator), DOMASTNodeUtils::getBinding);
		if( resp.level() == 0 && resp.canVisitChildren() ) {
			return true;
		}
		// always visit the type arguments though
		visitAllDescendentTypeArguments(node);
		return false;
	}
	
	private void visitAllDescendentTypeArguments(Type node) {
		// This feel suspect to me... maybe repeats nodes... idk yet
		node.accept(new ASTVisitor() {
			@Override
			public boolean visit(ParameterizedType node) {
				visitTypeArgumentList(node.typeArguments());
				return true;
			}
		});
	}
	
	protected void visitTypeArgumentList(List typeArgs) {
		ArrayList<Object> args = new ArrayList<Object>(typeArgs);
		for( Object t : args ) {
			((Type)t).accept(this);
		}
	}
	
	@Override
	public boolean visit(IntersectionType node) {
		return visitType(node);
	}
	@Override
	public boolean visit(UnionType node) {
		return visitType(node);
	}
	@Override
	public boolean visit(ClassInstanceCreation node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}
	@Override
	public boolean visit(CreationReference node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}
	@Override
	public boolean visit(SuperConstructorInvocation node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}
	@Override
	public boolean visit(SimpleName node) {
		if (
			node.getLocationInParent() == VariableDeclarationFragment.NAME_PROPERTY ||
			node.getLocationInParent() == SingleVariableDeclaration.NAME_PROPERTY ||
			node.getLocationInParent() == TypeDeclaration.NAME_PROPERTY ||
			node.getLocationInParent() == EnumDeclaration.NAME_PROPERTY ||
			node.getLocationInParent() == MethodDeclaration.NAME_PROPERTY) {
			return false; // skip as parent was most likely already matched
		}
		LocatorResponse resp = defaultVisitImplementationWithFunc(node, (x,y) -> y.match(node, this.nodeSet, this.locator), DOMASTNodeUtils::getBinding);
		return resp.level() == 0 && resp.canVisitChildren();
	}
	
	@Override
	public boolean visit(VariableDeclarationFragment node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}
	@Override
	public boolean visit(SingleVariableDeclaration node) {
		return defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
	}
	@Override
	public boolean visit(EnumConstantDeclaration node) {
		LocatorResponse response = getWrapper(this.patternLocator).match(node, this.nodeSet, this.locator);
		boolean mustResolve = (this.nodeSet.getWrapped().mustResolve || this.patternLocator.isMustResolve());
		int retLevel = response.level();
		if ((response.level() & PatternLocator.MATCH_LEVEL_MASK) == PatternLocator.POSSIBLE_MATCH && mustResolve) {
			LocatorResponse l1 = getWrapper(this.patternLocator).resolveLevel(node, node.resolveVariable(), this.locator);
			LocatorResponse l2 = getWrapper(this.patternLocator).resolveLevel(node, node.resolveConstructorBinding(), this.locator);
			retLevel = Math.max(l1.level(), l2.level());
		}
		this.nodeSet.addMatch(node, retLevel);
		return true;
	}
	@Override
	public boolean visit(QualifiedName node) {
		if (node.getLocationInParent() == SimpleType.NAME_PROPERTY) {
			return false; // type was already checked
		}
		LocatorResponse resp = defaultVisitImplementationWithFunc(node, (x,y) -> y.match(node, this.nodeSet, this.locator), DOMASTNodeUtils::getBinding);
		return resp.level() == 0 && resp.canVisitChildren();
	}

	@Override
	public boolean visit(ImportDeclaration node) {
		defaultVisitImplementation(node, (x,y) -> y.match(node, this.nodeSet, this.locator));
		return true;
	}
}
