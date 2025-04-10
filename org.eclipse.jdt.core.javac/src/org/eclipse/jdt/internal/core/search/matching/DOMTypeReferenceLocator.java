/*******************************************************************************
 * Copyright (c) 2023, 2024 Red Hat, Inc. and others.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *******************************************************************************/
package org.eclipse.jdt.internal.core.search.matching;

import static org.eclipse.jdt.internal.core.search.DOMASTNodeUtils.insideDocComment;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.ISourceRange;
import org.eclipse.jdt.core.ISourceReference;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.EnumConstantDeclaration;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IPackageBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.JdtCoreDomPackagePrivateUtility;
import org.eclipse.jdt.core.dom.LabeledStatement;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.QualifiedType;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.SearchMatch;
import org.eclipse.jdt.core.search.SearchPattern;
import org.eclipse.jdt.core.search.TypeDeclarationMatch;
import org.eclipse.jdt.core.search.TypeReferenceMatch;
import org.eclipse.jdt.internal.core.search.DOMASTNodeUtils;
import org.eclipse.jdt.internal.core.search.LocatorResponse;
import org.eclipse.jdt.internal.core.search.indexing.IIndexConstants;
import org.eclipse.jdt.internal.javac.dom.JavacTypeBinding;

public class DOMTypeReferenceLocator extends DOMPatternLocator {

	private TypeReferenceLocator locator;
	private List<IJavaElement> foundElements = new ArrayList<>();
	private Set<org.eclipse.jdt.core.dom.Name> imports = new HashSet<>();
	private MatchLocator matchLocator = null;
	private String packageName;

	public DOMTypeReferenceLocator(TypeReferenceLocator locator) {
		super(locator.pattern);
		this.locator = locator;
	}
	private boolean hasPackageDeclarationAncestor(org.eclipse.jdt.core.dom.ASTNode node) {
		if( node instanceof PackageDeclaration) {
			return true;
		}
		return node == null ? false : hasPackageDeclarationAncestor(node.getParent());
	}

	@Override
	public LocatorResponse match(org.eclipse.jdt.core.dom.Annotation node, NodeSetWrapper nodeSet, MatchLocator locator) {
		return match(node.getTypeName(), nodeSet, locator);
	}
	@Override
	public LocatorResponse match(Name name, NodeSetWrapper nodeSet, MatchLocator locator) {
		this.matchLocator = locator;
		if (name.getParent() instanceof AbstractTypeDeclaration) {
			return toResponse(IMPOSSIBLE_MATCH);
		}
		if( name.getParent() instanceof LabeledStatement ls && ls.getLabel() == name) {
			return toResponse(IMPOSSIBLE_MATCH);
		}
		if( name.getParent() instanceof BreakStatement bs && bs.getLabel() == name) {
			return toResponse(IMPOSSIBLE_MATCH);
		}
		if (failsFineGrain(name, this.locator.fineGrain())) {
			return toResponse(IMPOSSIBLE_MATCH);
		}
		if (this.locator.pattern.simpleName == null) {
			int v = this.locator.pattern.mustResolve ? POSSIBLE_MATCH : ACCURATE_MATCH;
			return toResponse(v, false);
		}
		if( name instanceof SimpleName sn2 ) {
			if( this.locator.pattern.qualification == null)
				return toResponse(match(sn2, nodeSet));
			// searching for a qualified name but we are only simple
			org.eclipse.jdt.core.dom.ASTNode parent3 = name.getParent();
			if( !(parent3 instanceof QualifiedName)) {
				return toResponse(match(sn2, nodeSet));
			}
			// Parent is a qualified name and we didn't match it...
			// so we know the whole name was a failed match, but...
			if( parent3 instanceof QualifiedName qn3 && qn3.getQualifier() == name) {
				// Maybe the qualifier is the type we're looking for
				if( match(sn2, nodeSet) == POSSIBLE_MATCH) {
					return toResponse(POSSIBLE_MATCH);
				}
			}

			if( this.locator.pattern.getMatchMode() == SearchPattern.R_EXACT_MATCH) {
				return toResponse(IMPOSSIBLE_MATCH);
			}
			if( match(sn2, nodeSet) == POSSIBLE_MATCH) {
				return toResponse(POSSIBLE_MATCH);
			}
			return toResponse(IMPOSSIBLE_MATCH);
		}
		if( name instanceof QualifiedName qn2 ) {
			return toResponse(match(qn2, nodeSet));
		}
		return toResponse(IMPOSSIBLE_MATCH);
	}
	@Override
	public LocatorResponse match(org.eclipse.jdt.core.dom.ASTNode node, NodeSetWrapper nodeSet, MatchLocator locator) {
		this.matchLocator = locator;
		if (failsFineGrain(node, this.locator.fineGrain())) {
			return toResponse(IMPOSSIBLE_MATCH);
		}
		if (node instanceof EnumConstantDeclaration enumConstantDecl
			&& node.getParent() instanceof EnumDeclaration enumDeclaration
			&& enumConstantDecl.getAnonymousClassDeclaration() != null) {
			if (this.locator.pattern.simpleName == null) {
				int v = nodeSet.addMatch(node, this.locator.pattern.mustResolve ? POSSIBLE_MATCH : ACCURATE_MATCH);
				return toResponse(v, true);
			}
			if (this.locator.matchesName(this.locator.pattern.simpleName, enumDeclaration.getName().getIdentifier().toCharArray())) {
				int v = nodeSet.addMatch(node, this.locator.pattern.mustResolve ? POSSIBLE_MATCH : ACCURATE_MATCH);
				return toResponse(v, true);
			}
		}
		if( node instanceof ImportDeclaration id) {
			Name n = id.getName();
			imports.add(n);
		}
		if( node instanceof PackageDeclaration pd) {
			this.packageName = pd.getName().toString();
		}
		return toResponse(IMPOSSIBLE_MATCH);
	}

	private LocatorResponse matchTypeNodeReturnComponent(Type node, String qualifiedNameFromNode, String fqqn,
			int defaultLevel) {
		if( this.locator.matchesName(qualifiedNameFromNode.toCharArray(), fqqn.toCharArray())) {
			Type nodeToUse = node;
			boolean replacementFound = false;
			if( !preferParamaterizedNode() && node instanceof ParameterizedType pt) {
				nodeToUse = pt.getType();
				replacementFound = true;
			}
			Type replacedNode = replacementFound ? nodeToUse : null;
			int typeParamMatches = validateTypeParameters(node);
			if( typeParamMatches == TYPE_PARAMS_MATCH) {
				return new LocatorResponse(defaultLevel, replacementFound, replacedNode, false, false);
			} else {
				int ret = typeParamMatches == TYPE_PARAMS_COUNT_MATCH ? ERASURE_MATCH : IMPOSSIBLE_MATCH;
				boolean isErasurePattern = isPatternErasureMatch();
				boolean isEquivPattern = isPatternEquivalentMatch();
				if( ret == ERASURE_MATCH && !isErasurePattern && !isEquivPattern)
					ret = IMPOSSIBLE_MATCH;
				return new LocatorResponse(ret, replacementFound, replacedNode, false, false);
			}
		}
		return null;
	}

	@Override
	public LocatorResponse match(Type node, NodeSetWrapper nodeSet, MatchLocator locator) {
		this.matchLocator = locator;
		if (failsFineGrain(node, this.locator.fineGrain())) {
			return toResponse(IMPOSSIBLE_MATCH);
		}
		int defaultLevel = this.locator.pattern.mustResolve ? POSSIBLE_MATCH : ACCURATE_MATCH;
		if (this.locator.pattern.simpleName == null) {
			int v = nodeSet.addMatch(node, defaultLevel);
			return toResponse(v, true);
		}
		String qualifiedNameFromNode = getQualifiedNameFromType(node);
		String simpleNameFromNode = getNameStringFromType(node);
		if( qualifiedNameFromNode != null && this.locator.pattern.qualification != null) {
			// we have a qualified name in the node, and our pattern is searching for a qualified name
			String patternQualifiedString = (new String(this.locator.pattern.qualification) + "." + new String(this.locator.pattern.simpleName));
			LocatorResponse r1 = matchTypeNodeReturnComponent(node, patternQualifiedString, qualifiedNameFromNode, defaultLevel);
			if( r1 != null ) return r1;

			// Not an exact match. We might need to check for more qualifications
			if( qualifiedNameFromNode.endsWith(patternQualifiedString)) {
				String[] patternQualifiedStringSegments = patternQualifiedString.split("\\.");
				String firstSegment = patternQualifiedStringSegments == null || patternQualifiedStringSegments.length == 0 ? null : patternQualifiedStringSegments[0];
				String fqqnImport = fqqnFromImport(firstSegment);
				if( fqqnImport != null ) {
					String fqqn = fqqnImport + patternQualifiedString.substring(firstSegment.length());
					r1 = matchTypeNodeReturnComponent(node, qualifiedNameFromNode, fqqn, defaultLevel);
					if( r1 != null ) return r1;
				}
			} else if( patternQualifiedString.endsWith(qualifiedNameFromNode)) {
				String[] qualifiedNameFromNodeStringSegments = qualifiedNameFromNode.split("\\.");
				String firstSegment = qualifiedNameFromNodeStringSegments == null || qualifiedNameFromNodeStringSegments.length == 0 ? null : qualifiedNameFromNodeStringSegments[0];
				String fqqnImport = fqqnFromImport(firstSegment);
				if( fqqnImport != null ) {
					String fqqn = fqqnImport + qualifiedNameFromNode.substring(firstSegment.length());
					r1 = matchTypeNodeReturnComponent(node, patternQualifiedString, fqqn, defaultLevel);
					if( r1 != null ) return r1;
				}
				if( this.packageName != null ) {
					String fqqn = this.packageName + "." + qualifiedNameFromNode;
					r1 = matchTypeNodeReturnComponent(node, patternQualifiedString, fqqn, defaultLevel);
					if( r1 != null ) return r1;
				}
			} else {
				String[] qualifiedNameFromNodeSegments = qualifiedNameFromNode.split("\\.");
				String[] qualifiedNamePatternSegments = patternQualifiedString.split("\\.");
				String firstNodeSegment = qualifiedNameFromNodeSegments == null ? null : qualifiedNameFromNodeSegments.length == 0 ? null : qualifiedNameFromNodeSegments[0];
				String firstPatternSegment = qualifiedNamePatternSegments == null ? null : qualifiedNamePatternSegments.length == 0 ? null : qualifiedNamePatternSegments[0];
				String fqqnImportFromNode = fqqnFromImport(firstNodeSegment);
				if( fqqnImportFromNode != null ) {
					String fqqn = fqqnImportFromNode + qualifiedNameFromNode.substring(firstNodeSegment.length());
					r1 = matchTypeNodeReturnComponent(node, qualifiedNameFromNode, fqqn, defaultLevel);
					if( r1 != null )
						return r1;
				}
				String fqqnImportFromPattern = fqqnFromImport(firstPatternSegment);
				if( fqqnImportFromPattern != null ) {
					String fqqn = fqqnImportFromPattern + patternQualifiedString.substring(firstPatternSegment.length());
					r1 = matchTypeNodeReturnComponent(node, qualifiedNameFromNode, fqqn, defaultLevel);
					if( r1 != null )
						return r1;
				}

			}
		} else if (simpleNameFromNode != null ) {
			if( this.locator.matchesName(this.locator.pattern.simpleName, simpleNameFromNode.toCharArray()) ) {
				int level = this.locator.pattern.mustResolve || this.locator.pattern.qualification == null ? POSSIBLE_MATCH : ACCURATE_MATCH;
				int typeParamMatches = validateTypeParameters(node);
				if( typeParamMatches == TYPE_PARAMS_NO_MATCH) level = IMPOSSIBLE_MATCH;
				if( typeParamMatches == TYPE_PARAMS_COUNT_MATCH) level = ERASURE_MATCH;

				if( isPatternExactMatch()) {
					if( typeParamMatches == TYPE_PARAMS_NO_MATCH) {
						boolean patternHasTypeArgs = this.locator.pattern.hasTypeArguments();
						List nodeTypeArgs = node instanceof ParameterizedType pt ? pt.typeArguments() : null;
						boolean nodeHasTypeArgs = nodeTypeArgs != null && nodeTypeArgs.size() > 0;
						if( !patternHasTypeArgs && !nodeHasTypeArgs) {
							return toResponse(level);
						}
						return new LocatorResponse(IMPOSSIBLE_MATCH, false, null, false, false);
					}
				}

				boolean isErasurePattern = isPatternErasureMatch();
				boolean isEquivPattern = isPatternEquivalentMatch();
				if( level == ERASURE_MATCH && !isErasurePattern && !isEquivPattern)
					level = IMPOSSIBLE_MATCH;

				if( level != IMPOSSIBLE_MATCH ) {
					if( !preferParamaterizedNode() || patternPrefersSimpleName()) {
						Name n = getSimpleNameNodeFromType(node);
						if( n != null ) {
							nodeSet.addMatch(n, level);
							return new LocatorResponse(level, true, n, true, true);
						}
					}
					int v = nodeSet.addMatch(node, level);
					return toResponse(v, true);
				}
			}
		}
		return toResponse(IMPOSSIBLE_MATCH);
	}

	private boolean patternPrefersSimpleName() {
		return false;
//		char[] qual = this.locator.pattern.qualification;
//		return qual == null;
	}
	private boolean isPatternErasureMatch() {
		int r = this.locator.pattern.getMatchRule();
		return (r & SearchPattern.R_ERASURE_MATCH) == SearchPattern.R_ERASURE_MATCH;
	}
	private boolean isPatternEquivalentMatch() {
		int r = this.locator.pattern.getMatchRule();
		return (r & SearchPattern.R_EQUIVALENT_MATCH) == SearchPattern.R_EQUIVALENT_MATCH;
	}
	private boolean isPatternExactMatch() {
		int r = this.locator.pattern.getMatchRule();
		return (r & SearchPattern.R_FULL_MATCH) == SearchPattern.R_FULL_MATCH;
	}


	private static final int TYPE_PARAMS_MATCH = 1;
	private static final int TYPE_PARAMS_COUNT_MATCH = 2;
	private static final int TYPE_PARAMS_NO_MATCH = 3;

	private int validateTypeParameters(Type node) {
		// SimpleType with typeName=QualifiedName
		boolean patternHasTypeArgs = this.locator.pattern.hasTypeArguments();
		//boolean patternHasTypeParameters = this.locator.pattern.hasTypeParameters();
		boolean erasureMatch = isPatternErasureMatch();
		boolean equivMatch = isPatternEquivalentMatch();
		boolean exactMatch = isPatternExactMatch();
		if( patternHasTypeArgs && !(erasureMatch || equivMatch || exactMatch )) {
			return TYPE_PARAMS_NO_MATCH;
		}

		char[][][] fromPattern = this.locator.pattern.getTypeArguments();
		if( fromPattern == null ) {
			return TYPE_PARAMS_MATCH;
		}
		if( node instanceof SimpleType st && (st.getName() instanceof QualifiedName || st.getName() instanceof SimpleName)) {
			if( !erasureMatch && !equivMatch) {
				for( int i = 0; i < fromPattern.length; i++ ) {
					if( fromPattern[i] == null || fromPattern[i].length != 0 ) {
						return TYPE_PARAMS_NO_MATCH;
					}
				}
			}
			return TYPE_PARAMS_MATCH;
		}

		Type working = node;
		boolean done = false;
		int i = 0;
		for( i = 0; i < fromPattern.length && !done; i++ ) {
			char[][] thisLevelTypeParams = fromPattern[i];
			List typeArgs = working instanceof ParameterizedType pt ? pt.typeArguments() : null;
			boolean emptyPatternParams = thisLevelTypeParams == null || thisLevelTypeParams.length == 0;
			if( emptyPatternParams) {
				if( exactMatch && emptyPatternParams && (typeArgs != null && typeArgs.size() > 0) ) {
					return TYPE_PARAMS_NO_MATCH;
				}
			} else {
				if( typeArgs == null || typeArgs.size() != thisLevelTypeParams.length) {
					return TYPE_PARAMS_NO_MATCH;
				}
				for( int j = 0; j < thisLevelTypeParams.length; j++ ) {
					String patternSig = new String(thisLevelTypeParams[j]);
					IBinding patternBinding = JdtCoreDomPackagePrivateUtility.findBindingForType(node, patternSig);
					if( patternBinding == null ) {
						boolean plusOrMinus = patternSig.startsWith("+") || patternSig.startsWith("-");
						String safePatternString = plusOrMinus ? patternSig.substring(1) : patternSig;
						if( safePatternString.startsWith("Q")) {
							patternBinding = JdtCoreDomPackagePrivateUtility.findUnresolvedBindingForType(node, safePatternString);
						}
					}
					ASTNode argj = (ASTNode)typeArgs.get(j);
					IBinding domBinding = DOMASTNodeUtils.getBinding(argj);
					ITypeBinding domTypeBinding = domBinding instanceof ITypeBinding ? (ITypeBinding)domBinding : null;
					String domSig = domBinding == null ? null : domBinding instanceof JavacTypeBinding jctb ? jctb.getGenericTypeSignature(false) : domBinding.getKey();

					if( exactMatch ) {
						if( patternSig.equals(domSig)) {
							continue;
						}
						if( patternSig.equals("*") && isQuestionMark(domSig)) {
							continue;
						}
						return TYPE_PARAMS_COUNT_MATCH;
					}

					if( patternSig.equals(("*")))
						continue;
					if( patternSig.equals(domSig)) {
						continue;
					}

					String patternKeyFromBinding = patternBinding == null ? null : patternBinding instanceof JavacTypeBinding jctb ? jctb.getGenericTypeSignature(false) : patternBinding.getKey();
					List<IBinding> patternAncestors = new ArrayList<>();
					if( patternBinding instanceof ITypeBinding patternTypeBinding) {
						if( patternSig.startsWith("+") || patternSig.startsWith("-")) {
							patternAncestors = findAllSuperclassAndInterfaceBindingsForWildcard(patternTypeBinding);
						} else {
							patternAncestors = findAllSuperclassAndInterfaceBindings(patternTypeBinding);
						}
					}

					List<String> patternAncestorKeys = patternAncestors.stream().map(x -> x instanceof JavacTypeBinding jctb ? jctb.getGenericTypeSignature(false) : x.getKey()).collect(Collectors.toList());
					if( patternSig.startsWith("-")) {
						if( domSig.startsWith("-") || !domSig.startsWith("+")) {
							String domKey = domBinding instanceof JavacTypeBinding jctb ? jctb.getGenericTypeSignature(false) : domBinding.getKey();
							if( !patternAncestorKeys.contains(domKey)) {
								return TYPE_PARAMS_COUNT_MATCH;
							}
						} else if( domSig.startsWith("+") && !isQuestionMark(domSig)) {
							return TYPE_PARAMS_COUNT_MATCH;
						}
					} else if( patternSig.startsWith("+")) {
						if( domSig.startsWith("-")) {
							// There's no way ALL ancestors of dom can be a subclass of pattern unless pattern is java.lang.Object
							return TYPE_PARAMS_COUNT_MATCH;
						} else {
							List<IBinding> domHeirarchy = findAllSuperclassAndInterfaceBindingsForWildcard(domTypeBinding);
							List<String> domHeirarchyStrings = domHeirarchy.stream().map(x -> x instanceof JavacTypeBinding jctb ? jctb.getGenericTypeSignature(false) : x.getKey()).collect(Collectors.toList());
							if( patternKeyFromBinding != null ) {
								if( !resolvedPatternMatchesDom(patternSig.substring(1), patternKeyFromBinding, domTypeBinding, domHeirarchyStrings)) {
									return TYPE_PARAMS_COUNT_MATCH;
								}
							} else {
								if( !unresolvedPatternMatchesDom(patternSig, domSig, domTypeBinding, domHeirarchyStrings)) {
									return TYPE_PARAMS_COUNT_MATCH;
								}
							}
						}
					} else {
						// pattern is a normal defined type, ex:  Exception
						if( domSig.startsWith("-")) {
							List<IBinding> domHeirarchy = findAllSuperclassAndInterfaceBindingsForWildcard(domTypeBinding);
							List<String> domHeirarchyStrings = domHeirarchy.stream().map(x -> x instanceof JavacTypeBinding jctb ? jctb.getGenericTypeSignature(false) : x.getKey()).collect(Collectors.toList());
							if( patternKeyFromBinding == null && !domHeirarchyStrings.contains(patternKeyFromBinding))
								return TYPE_PARAMS_COUNT_MATCH;
						} else if( domSig.startsWith("+")) {
							if( domTypeBinding != null ) {
								ITypeBinding bound = domTypeBinding.getBound();
								String boundKey = bound == null ? null : bound instanceof JavacTypeBinding jctb ? jctb.getGenericTypeSignature(false) : bound.getKey();
								if( !isQuestionMark(domSig) && (boundKey == null || !patternAncestorKeys.contains(boundKey))) {
									return TYPE_PARAMS_COUNT_MATCH;
								}
							}
						} else {
							// Just two normal param types, see if they match
							if( !patternSig.equals(domSig)) {
								if( !unresolvedPatternMatchesDom(patternSig, domSig, domTypeBinding, new ArrayList<>())) {
									return TYPE_PARAMS_COUNT_MATCH;
								}
							}
						}
					}
				}
			}
			if( working instanceof ParameterizedType ptt)
				working = ptt.getType();
			if( working instanceof QualifiedType qtt) {
				working = qtt.getQualifier();
			}
			if( working instanceof SimpleType)
				done = true;
		}
		for( int k = i; k < fromPattern.length; k++ ) {
			if( fromPattern[k] != null && fromPattern[k].length != 0) {
				// More typeargs required, but we don't have any more
				return TYPE_PARAMS_NO_MATCH;
			}
		}
		return TYPE_PARAMS_MATCH;
	}

	private boolean resolvedPatternMatchesDom(String patternSig, String patternKeyFromBinding, ITypeBinding domTypeBinding,
			List<String> domHeirarchyStrings) {
		String k = domTypeBinding instanceof JavacTypeBinding jctb ? jctb.getGenericTypeSignature(false) : domTypeBinding.getKey();
		if( isQuestionMark(k))
			return true;
		if( domHeirarchyStrings.contains(patternSig))
			return true;
		if( patternKeyFromBinding != null) {
			if( domHeirarchyStrings.contains(patternKeyFromBinding))
				return true;
			if( patternKeyFromBinding.startsWith("+") && domHeirarchyStrings.contains(patternKeyFromBinding.substring(1))) {
				return true;
			}
		}
		return false;
	}

	private boolean unresolvedPatternMatchesDom(String patternSig, String domSig, ITypeBinding domTypeBinding,
			List<String> domHeirarchyStrings) {
		boolean patternSigIsUnresolved = false;
		String patternSigTrimmed = null;
		if( patternSig.startsWith("Q")) {
			patternSigTrimmed = patternSig.substring(1);
			patternSigIsUnresolved = true;
		} else if( patternSig.startsWith("+Q")) {
			patternSigTrimmed = patternSig.substring(2);
			patternSigIsUnresolved = true;
		}
		if( patternSigIsUnresolved) {
			// TODO this is insufficient
			if( domSig.endsWith("." + patternSigTrimmed))
				return true;
		}

		String patternSigWithoutPrefix = patternSig.startsWith("+") || patternSig.startsWith("-") ? patternSig.substring(1) : patternSig;
		String domSigWithoutPrefix = domSig.startsWith("+") || domSig.startsWith("-") ? domSig.substring(1) : domSig;
		String patternSig2 = patternSigWithoutPrefix.substring(1);
		if( patternSig2.equals(domSigWithoutPrefix.substring(1)))
			return true;
		if( domSig.endsWith("." + patternSig2) )
			return true;

		return false;
	}
	private boolean isQuestionMark(String k) {
		// wut ?
		boolean isQuestionMark = "+Ljava/lang/Object;".equals(k) || "+Qjava.lang.Object;".equals(k);
		return isQuestionMark;
	}
	private List<IBinding> findAllSuperclassAndInterfaceBindingsForWildcard(ITypeBinding binding) {
		List<IBinding> ret = new ArrayList<>();
		if( binding == null )
			return ret;
		// Sometimes we have a discovered binding which is already the bound...
		ITypeBinding param = binding.isWildcardType() ? binding.getBound() : binding;
		if( param != null ) {
			ret.add(param);
			fillAllSuperclassAndInterfaceBindings(param, ret);
		} else {
			// for pure `?`
			ret.add(binding);
		}
		return ret;
	}
	private List<IBinding> findAllSuperclassAndInterfaceBindings(ITypeBinding binding) {
		List<IBinding> ret = new ArrayList<>();
		if( binding == null )
			return ret;
		fillAllSuperclassAndInterfaceBindings(binding, ret);
		return ret;
	}
	private void fillAllSuperclassAndInterfaceBindings(ITypeBinding binding, List<IBinding> list) {
		if( binding == null )
			return;
		ITypeBinding[] ifaces = binding.getInterfaces();
		for( int q = 0; q < ifaces.length; q++ ) {
			ITypeBinding oneInterface = ifaces[q];
			if( oneInterface != null ) {
				list.add(oneInterface);
				fillAllSuperclassAndInterfaceBindings(oneInterface, list);
			}
		}
		ITypeBinding superClaz = binding.getSuperclass();
		if( superClaz != null ) {
			list.add(superClaz);
			fillAllSuperclassAndInterfaceBindings(superClaz, list);
		}
	}

	private String getQualifiedNameFromType(Type query) {
		if( query instanceof QualifiedType qtt) {
			String qualString = getQualifiedNameFromType(qtt.getQualifier());
			String nameString = getNameStringFromType(query);
			return qualString == null || nameString == null ? null :
				qualString + "." + nameString;
		}
		if( query instanceof ParameterizedType ptt) {
			return getQualifiedNameFromType(ptt.getType());
		}
		if( query instanceof SimpleType st) {
			String fqqn = fqqnFromImport(st.getName().toString());
			return fqqn != null ? fqqn : st.getName().toString();
		}
		return null;
	}

	private String getNameStringFromType(Type node) {
		Name nnode = getNameNodeFromType(node);
		nnode = (nnode instanceof QualifiedName qn ? qn.getName() : nnode);
		return getNameStringFromNameObject(nnode);
	}

	private String getNameStringFromNameObject(Name nnode) {
		if (nnode instanceof SimpleName name) {
			return name.getIdentifier();
		}
		return nnode == null ? null : nnode.toString();
	}

	private org.eclipse.jdt.core.dom.Name getSimpleNameNodeFromType(Type node) {
		org.eclipse.jdt.core.dom.Name name = getNameNodeFromType(node);
		if( name != null ) {
			if( name instanceof QualifiedName qn ) {
				return qn.getName();
			}
			return name;
		}
		return null;
	}

	private org.eclipse.jdt.core.dom.Name getNameNodeFromType(Type node) {
		if (node instanceof SimpleType simple) {
			return simple.getName();
		} else if (node instanceof QualifiedType qualified) {
			return qualified.getName();
		} else if( node instanceof ParameterizedType ptt) {
			return getNameNodeFromType(ptt.getType());
		}
		return null;
	}

	private org.eclipse.jdt.core.dom.Name getQualifierNameNodeFromType(Type node) {
		if (node instanceof SimpleType simple) {
			return simple.getName();
		} else if (node instanceof QualifiedType qualified) {
			return getQualifierNameNodeFromType(qualified.getQualifier());
		} else if( node instanceof ParameterizedType ptt) {
			return getQualifierNameNodeFromType(ptt.getType());
		}
		return null;
	}


	private String fqqnFromImport(String firstSegment) {
		if( firstSegment == null )
			return null;

		for( Name n : imports ) {
			if( n.isSimpleName() && n.toString().equals(firstSegment)) {
				return n.toString();
			}
			if( n.isQualifiedName() && n.toString().endsWith("." + firstSegment)) {
				return n.toString();
			}
		}
		return null;
	}
	@Override
	public LocatorResponse resolveLevel(org.eclipse.jdt.core.dom.ASTNode node, IBinding binding, MatchLocator locator) {
		if (binding == null) {
			if( node instanceof SimpleName sn) {
				int accuracy = resolveLevelForSimpleName(node, sn.getIdentifier());
				if( accuracy != -1 ) {
					// Add directly
					IResource r = null;
					IJavaElement enclosing = DOMASTNodeUtils.getEnclosingJavaElement(node);
					IJavaElement ancestor = enclosing == null ? null : enclosing.getAncestor(IJavaElement.COMPILATION_UNIT);
					try {
						r = ancestor == null ? null : ancestor.getCorrespondingResource();
					} catch(JavaModelException jme) {
						// ignore
					}

					TypeReferenceMatch typeMatch = new TypeReferenceMatch(enclosing, accuracy, node.getStartPosition(), node.getLength(), insideDocComment(node), locator.getParticipant(), r);
					try {
						locator.report(typeMatch);
					} catch(CoreException ce) {
						// ignore
					}
					// Then return not possible so it doesn't get added again
					return toResponse(IMPOSSIBLE_MATCH);
				}
			}
			return toResponse(INACCURATE_MATCH);
		}
		if (binding instanceof ITypeBinding typeBinding) {
			if( hasImportAncestor(node) && !this.locator.isDeclarationOfReferencedTypesPattern) {
				return resolveLevelForImportBinding(node, typeBinding, locator);
			}
			boolean patternHasTypeArgs = this.locator.pattern.hasTypeArguments();
			boolean patternHasTypeParameters = this.locator.pattern.hasTypeParameters();
			boolean patternHasSignatures = this.locator.pattern.hasSignatures();
			boolean erasureMatch = isPatternErasureMatch();
			boolean equivMatch = isPatternEquivalentMatch();
			boolean exactMatch = isPatternExactMatch();
			if( (patternHasTypeArgs && !(erasureMatch || equivMatch || exactMatch))) {
				return toResponse(IMPOSSIBLE_MATCH);
			}
			int v = resolveLevelForTypeBinding(node, typeBinding, locator);
			if( node instanceof ParameterizedType pt) {
				ASTNode n = preferParamaterizedNode() ? pt : pt.getType();
				return new LocatorResponse(v, n != pt, n, false, false);
			}
			if( patternHasTypeArgs && !patternHasSignatures && !erasureMatch) {
				// the search doesn't have type args in it, but the type does
				return toResponse(IMPOSSIBLE_MATCH);
			}
			return toResponse(v);
		}
		if( binding instanceof IPackageBinding && node instanceof SimpleName sn) {
			// var x = (B36479.C)val;
			// might interpret the B36479 to be a package and C a type,
			// rather than B36479 to be a type and C to be an inner-type
			if( this.locator.isDeclarationOfReferencedTypesPattern) {
				return toResponse(IMPOSSIBLE_MATCH);
			}
			if( hasPackageDeclarationAncestor(node)) {
				return toResponse(IMPOSSIBLE_MATCH);
			}
			String identifier = sn.getIdentifier();
			if( this.locator.matchesName(this.locator.pattern.simpleName, identifier.toCharArray())) {
				return toResponse(INACCURATE_MATCH);
			}

		}
		return toResponse(IMPOSSIBLE_MATCH);
	}

	private boolean preferParamaterizedNode() {
		int patternRule = this.locator.pattern.getMatchRule();
		boolean patternIsErasureMatch = isPatternErasureMatch();
		boolean patternIsEquivMatch = isPatternEquivalentMatch();

		boolean hasTypeArgs = this.locator.pattern.hasTypeArguments();
		boolean hasTypeParams = this.locator.pattern.hasTypeParameters();
		boolean emptyTypeArgsPattern = this.locator.pattern.getTypeArguments() == null ||
				this.locator.pattern.getTypeArguments().length == 0;
		if( patternIsEquivMatch)
			return hasTypeArgs;
		if( patternIsErasureMatch) {
			return false;
		}
//		if( emptyTypeArgsPattern || patternIsErasureMatch || patternIsEquivMatch ) {
//			return false;
//		}
		return true;
	}

	private LocatorResponse resolveLevelForImportBinding(ASTNode node, ITypeBinding typeBinding,
			MatchLocator locator2) {
		if (this.locator.pattern.hasTypeArguments() && !this.isEquivalentMatch &&!this.isErasureMatch) {
			return toResponse(0);
		}

		// Return if fine grain is on and does not concern import reference
		if ((this.locator.pattern.fineGrain != 0 && (this.locator.pattern.fineGrain & IJavaSearchConstants.IMPORT_DECLARATION_TYPE_REFERENCE) == 0)) {
			return toResponse(0);
		}
		int newLevel = this.resolveLevelForTypeFQN(this.locator.pattern.simpleName,
				this.locator.pattern.qualification, typeBinding, null);
		return toResponse(newLevel);
	}
	private static boolean failsFineGrain(ASTNode node, int fineGrain) {
		if (fineGrain == 0) {
			return false;
		}
		if ((fineGrain & IJavaSearchConstants.INSTANCEOF_TYPE_REFERENCE) != 0) {
			ASTNode cursor = node;
			while (cursor != null && !(cursor instanceof InstanceofExpression)) {
				cursor = cursor.getParent();
			}
			if (cursor == null) {
				return true;
			}
		}
		return false;
	}

	/*
	 * Returns a match flag OR -1 if it cannot determine at all.
	 */
	private int resolveLevelForSimpleName(org.eclipse.jdt.core.dom.ASTNode node, String simpleNameNeedle) {
		if( !simpleNameNeedle.contains(".") && this.locator.pattern.qualification != null && this.locator.pattern.qualification.length > 0 ) { //$NON-NLS-1$
			// we need to find out if we import this thing at all
			for( Name id : imports) {
				if( id instanceof QualifiedName qn) {
					if( qn.getName().toString().equals(simpleNameNeedle)) {
						char[] qualifiedPattern = this.locator.getQualifiedPattern(this.locator.pattern.simpleName, this.locator.pattern.qualification);
						// we were imported as qualified name...
						int level3 = this.resolveLevelForTypeSourceName(qualifiedPattern, qn.toString().toCharArray(), null);
						if( level3 == ACCURATE_MATCH ) {
							return INACCURATE_MATCH;
						}
						return INACCURATE_MATCH;
					}
				}
			}
		}
		return -1;
	}

	private int resolveLevelForTypeBinding(org.eclipse.jdt.core.dom.ASTNode node, ITypeBinding typeBinding,
			MatchLocator locator) {
		IImportDiscovery importDiscovery = new IImportDiscovery() {
			@Override
			public String findImportForString(String s) {
				return fqqnFromImport(s);
			}
		};

		int newLevel = this.resolveLevelForTypeFQN(this.locator.pattern.simpleName,
				this.locator.pattern.qualification, typeBinding, importDiscovery);
		if( this.locator.isDeclarationOfReferencedTypesPattern) {
			return resolveLevelForTypeBindingDeclarationOfReferencedTypes(typeBinding, node, newLevel, locator);
		}
		if( newLevel == IMPOSSIBLE_MATCH ) {
			String qualNameFromBinding = typeBinding.getQualifiedName();
			int simpleNameMatch = resolveLevelForSimpleName(node, qualNameFromBinding);
			if( simpleNameMatch != -1 ) {
				return simpleNameMatch;
			}
		}
		if( newLevel == ACCURATE_MATCH ) {
			if( this.locator.pattern.hasTypeArguments() ) {
				return resolveLevelForTypeBindingWithTypeArguments(typeBinding, node, locator);
			}
		}
		return newLevel;
	}

	private int resolveLevelForTypeBindingWithTypeArguments(ITypeBinding typeBinding, ASTNode node,
			MatchLocator locator2) {
		boolean patternHasTypeArgs = this.locator.pattern.hasTypeArguments();
		boolean patternHasTypeParams = this.locator.pattern.hasTypeParameters();
		boolean patternHasTypeSignatures = !(patternHasTypeArgs && patternHasTypeParams);

		char[][][] patternTypeArgArray = this.locator.pattern.getTypeArguments();
		int patternTypeArgsLength = patternTypeArgArray == null ? -1 :
			patternTypeArgArray[0] == null ? -1 :
				patternTypeArgArray[0].length;
		boolean bindingIsRaw = typeBinding.isRawType();
		boolean bindingIsGeneric = typeBinding.isGenericType();
		boolean bindingIsParameterized = typeBinding.isParameterizedType();
		int patternRule = this.locator.pattern.getMatchRule();
		boolean patternIsErasureMatch = isPatternErasureMatch();
		boolean patternIsEquivMatch = isPatternEquivalentMatch();

		ITypeBinding[] bindingArgs = typeBinding.getTypeArguments();
		ITypeBinding[] bindingParams = typeBinding.getTypeParameters();
		int bindingTypeArgsLength = bindingArgs == null ? -1 : bindingArgs.length;
		// Compare arguments lengths
		if (patternTypeArgsLength == bindingTypeArgsLength) {
			Type t = node instanceof Type ? (Type)node : null;
			if( t != null ) {
				int typeArgsValidation = validateTypeParameters(t);
				if( typeArgsValidation == TYPE_PARAMS_MATCH) {
					if( !patternHasTypeSignatures) {
						return ERASURE_MATCH;
					}
					return ACCURATE_MATCH;
				}

				if( typeArgsValidation == TYPE_PARAMS_COUNT_MATCH || typeArgsValidation == TYPE_PARAMS_NO_MATCH) {
					if( isPatternExactMatch()) {
						return IMPOSSIBLE_MATCH;
					} else {
						return ERASURE_MATCH;
					}
				}
			}
			if (!bindingIsRaw && patternHasTypeArgs) {
				// generic patterns are always not compatible match
				return ERASURE_MATCH;
			}
			return ACCURATE_MATCH;
		} else {
			if (patternTypeArgsLength==0) {
				return ACCURATE_MATCH;
			} else if (bindingTypeArgsLength==0) {
				// If this is an import, we have to treat it differently

				// pattern looking for args but binding has none.
//				ITypeBinding decl = typeBinding.getTypeDeclaration();
//				ITypeBinding[] declArgs = decl.getTypeArguments();
//				ITypeBinding[] declParams = decl.getTypeParameters();
				// raw binding is always compatible
				if( patternIsEquivMatch && bindingIsRaw) {
					return ACCURATE_MATCH;
				}
				if( !bindingIsRaw && !(patternIsEquivMatch || patternIsErasureMatch)) {
					return IMPOSSIBLE_MATCH;
				}
				if( !patternIsEquivMatch || bindingIsRaw)
					return ACCURATE_MATCH;
			}
		}
		return IMPOSSIBLE_MATCH;
	}
	private int resolveLevelForTypeBindingDeclarationOfReferencedTypes(ITypeBinding typeBinding, ASTNode node, int newLevel, MatchLocator locator) {
		IJavaElement enclosing = ((DeclarationOfReferencedTypesPattern)this.locator.pattern).enclosingElement;
		// We don't add this node. We manually add the declaration
		ITypeBinding t2 = typeBinding.getTypeDeclaration();
		IJavaElement je = t2 == null ? null : t2.getJavaElement();
		if( je != null && !this.foundElements.contains(je) && DOMASTNodeUtils.isWithinRange(node, enclosing)) {
			ISourceReference sr = je instanceof ISourceReference ? (ISourceReference)je : null;
			IResource r = null;
			ISourceRange srg = null;
			ISourceRange nameRange = null;
			try {
				srg = sr.getSourceRange();
				nameRange = sr.getNameRange();
				IJavaElement ancestor = je.getAncestor(IJavaElement.COMPILATION_UNIT);
				r = ancestor == null ? null : ancestor.getCorrespondingResource();
			} catch(JavaModelException jme) {
				// ignore
			}
			ISourceRange rangeToUse = (nameRange == null) ? srg : nameRange;
			if( rangeToUse != null ) {
				TypeDeclarationMatch tdm = new TypeDeclarationMatch(je, newLevel,
						rangeToUse.getOffset(), rangeToUse.getLength(),
						locator.getParticipant(), r);
				try {
					this.foundElements.add(je);
					locator.report(tdm);
				} catch(CoreException ce) {
					// ignore
				}
			}
		}
		return IMPOSSIBLE_MATCH;
	}
	private org.eclipse.jdt.core.dom.CompilationUnit findCU(org.eclipse.jdt.core.dom.ASTNode node) {
		if( node == null )
			return null;
		if( node instanceof org.eclipse.jdt.core.dom.CompilationUnit cu) {
			return cu;
		}
		return findCU(node.getParent());
	}

	public int match(SimpleName name, NodeSetWrapper nodeSet) {
		String simpleName = name.getIdentifier();
		return simpleName != null && this.locator.matchesName(this.locator.pattern.simpleName, simpleName.toCharArray()) ?
			POSSIBLE_MATCH : IMPOSSIBLE_MATCH;
	}
	public int match(QualifiedName name, NodeSetWrapper nodeSet) {
		String simpleName = name.getName().getIdentifier();
		String qualifier = name.getQualifier().toString();
		if( this.locator.pattern.qualification == null ) {
			// Return an impossible match here, because we are not seeking a qualifier.
			// The SimpleName node should be the one to respond.
			return IMPOSSIBLE_MATCH;
		}
		if( qualifier != null) {
			String desiredQualifier = new String(this.locator.pattern.qualification);
			if( !qualifier.equals(desiredQualifier)) {
				return IMPOSSIBLE_MATCH;
			}
		}
		return simpleName != null && this.locator.matchesName(this.locator.pattern.simpleName, simpleName.toCharArray()) ?
			POSSIBLE_MATCH : IMPOSSIBLE_MATCH;
	}
	protected int resolveLevelForType(ITypeBinding typeBinding) {
		if (typeBinding == null) {
			if (this.locator.pattern.typeSuffix != IIndexConstants.TYPE_SUFFIX) return INACCURATE_MATCH;
		} else {
			switch (this.locator.pattern.typeSuffix) {
				case IIndexConstants.CLASS_SUFFIX:
					if (!typeBinding.isClass()) return IMPOSSIBLE_MATCH;
					break;
				case IIndexConstants.CLASS_AND_INTERFACE_SUFFIX:
					if (!(typeBinding.isClass() || (typeBinding.isInterface() && !typeBinding.isAnnotation()))) return IMPOSSIBLE_MATCH;
					break;
				case IIndexConstants.CLASS_AND_ENUM_SUFFIX:
					if (!(typeBinding.isClass() || typeBinding.isEnum())) return IMPOSSIBLE_MATCH;
					break;
				case IIndexConstants.INTERFACE_SUFFIX:
					if (!typeBinding.isInterface() || typeBinding.isAnnotation()) return IMPOSSIBLE_MATCH;
					break;
				case IIndexConstants.INTERFACE_AND_ANNOTATION_SUFFIX:
					if (!(typeBinding.isInterface() || typeBinding.isAnnotation())) return IMPOSSIBLE_MATCH;
					break;
				case IIndexConstants.ENUM_SUFFIX:
					if (!typeBinding.isEnum()) return IMPOSSIBLE_MATCH;
					break;
				case IIndexConstants.ANNOTATION_TYPE_SUFFIX:
					if (!typeBinding.isAnnotation()) return IMPOSSIBLE_MATCH;
					break;
				case IIndexConstants.TYPE_SUFFIX : // nothing
			}
		}
		return this.resolveLevelForType(this.locator.pattern.simpleName,
							this.locator.pattern.qualification,
							typeBinding);
	}

	private boolean hasImportAncestor(ASTNode node) {
		ASTNode working = node;
		while(working != null) {
			if( working instanceof ImportDeclaration )
				return true;
			working = working.getParent();
		}
		return false;
	}

	@Override
	public void reportSearchMatch(MatchLocator locator, ASTNode node, SearchMatch match) throws CoreException {
//		boolean matchIsEr = match.isErasure();
//		boolean matchIsEq = match.isEquivalent();
//		boolean matchIsEx = match.isExact();
		boolean report = (this.isErasureMatch && match.isErasure()) || (this.isEquivalentMatch && match.isEquivalent()) || match.isExact();
		if (!report)
			return;

		updateMatchPositions(node, match);
		updateMatchRule(node, match);

		report = (this.isErasureMatch && match.isErasure()) || (this.isEquivalentMatch && match.isEquivalent()) || match.isExact();
		if (!report)
			return;
		// fix org.eclipse.jdt.core.tests.model.JavaSearchBugsTests.testBug83804_Type()
		Object object = match.getElement();
		if ( (object instanceof org.eclipse.jdt.internal.core.CompilationUnit unit) && unit.getPackageName() != null) {
			String pkgName = CharOperation.toString(unit.getPackageName());
			IJavaElement element = unit.getPackageDeclaration(pkgName);
			match.setElement(element);
		}
		SearchMatchingUtility.reportSearchMatch(locator, match);
	}


	private void updateMatchRule(ASTNode node, SearchMatch match) {
		if( hasImportAncestor(node))
			return;

		// Compare arguments lengthes
		int matchRule = match.getRule();
		char[][][] fromPattern = this.locator.pattern.getTypeArguments();
		int patternTypeArgsLength = (fromPattern == null || fromPattern.length == 0 || fromPattern[0] == null ? 0 : fromPattern.length <= 0 ? 0 : fromPattern[0].length);
		int typeArgumentsLength = node instanceof ParameterizedType ptt ? ptt.typeArguments().size() : 0;
		boolean hasTypeParameters = this.locator.pattern.hasTypeParameters();

		if (match.isRaw()) {
			if (patternTypeArgsLength != 0) {
				matchRule &= ~SearchPattern.R_FULL_MATCH;
			}
		}
		if (hasTypeParameters) {
			matchRule = SearchPattern.R_ERASURE_MATCH;
		}



		if (patternTypeArgsLength == typeArgumentsLength) {
			if (!match.isRaw() && hasTypeParameters) {
				// generic patterns are always not compatible match
				match.setRule(SearchPattern.R_ERASURE_MATCH);
			}
		} else {
			if (patternTypeArgsLength==0) {
				if (!match.isRaw() || hasTypeParameters) {
					match.setRule(matchRule & ~SearchPattern.R_FULL_MATCH);
				}
			} else  if (typeArgumentsLength==0) {
				// raw binding is always compatible
				match.setRule(matchRule & ~SearchPattern.R_FULL_MATCH);
			} else {
				match.setRule(0); // impossible match
				return;
			}
		}
		if (fromPattern == null || fromPattern.length == 0 || fromPattern[0] == null) {
			match.setRule(matchRule);
		}
	}
	private void updateMatchPositions(ASTNode node, SearchMatch match) {
		ASTNode replacementNodeForStartPosition = null;
		ASTNode toCheck = node;
		if( node instanceof ParameterizedType ptt) {
			toCheck = ptt.getType();
		}
		if( toCheck instanceof QualifiedType qtt) {
			replacementNodeForStartPosition = findNodeMatchingPatternQualifier(qtt.getQualifier());
		} else if( toCheck instanceof SimpleType st) {
			replacementNodeForStartPosition = findNodeMatchingPatternQualifier(st);
		}

		if( replacementNodeForStartPosition != null ) {
			int matchStart = match.getOffset();
			int matchEnd = matchStart + match.getLength();
			int newStart = replacementNodeForStartPosition.getStartPosition();
			int newLength = matchEnd - newStart;
			match.setOffset(newStart);
			match.setLength(newLength);
		}

		ASTNode working = (replacementNodeForStartPosition != null ? replacementNodeForStartPosition : node);
		int trimQualifierStart = findTrimQualifierStart(working);
		if( trimQualifierStart != -1 ) {
			int matchStart = match.getOffset();
			int matchEnd = matchStart + match.getLength();
			int newStart = trimQualifierStart;
			int newLength = matchEnd - newStart;
			match.setOffset(newStart);
			match.setLength(newLength);
		}
	}
	private int findTrimQualifierStart(ASTNode working) {
		String needle = this.locator.pattern.qualification == null ? null : new String(this.locator.pattern.qualification);
		if( needle == null && working instanceof Type workingType) {
			ASTNode n1 = getSimpleNameNodeFromType(workingType);
			if( n1 != null ) {
				return n1.getStartPosition();
			}
		}
		if( needle != null && working instanceof Type workingType ) {
			Name n = getQualifierNameNodeFromType(workingType);
			n = (n instanceof QualifiedName qn ? qn.getName() : n);
			String asString = getNameStringFromNameObject(n);
			if( asString != null ) {
				if (this.locator.matchesName(this.locator.pattern.qualification, asString.toCharArray())) {
					return n.getStartPosition();
				}
			}
		}
		return -1;
	}
	private ASTNode findNodeMatchingPatternQualifier(Type qualifier) {
		if( qualifier instanceof ParameterizedType pt) {
			return findNodeMatchingPatternQualifier(pt.getType());
		}
		Name[] retNode = new Name[] {null};
		String needle = this.locator.pattern.qualification == null ? null : new String(this.locator.pattern.qualification);
		if( needle != null ) {
			qualifier.accept(new ASTVisitor() {
				@Override
				public boolean visit(QualifiedName node) {
					if( node.getName().toString().equals(needle)) {
						retNode[0] = node.getName();
					}
					return retNode[0] == null;
				}
				@Override
				public boolean visit(SimpleName node) {
					if( node.toString().equals(needle)) {
						retNode[0] = node;
					}
					return retNode[0] == null;
				}
				@Override
				public boolean visit(ParameterizedType node) {
					node.getType().accept(this);
					return false;
				}
			});
		}
		return retNode[0];
	}

}
