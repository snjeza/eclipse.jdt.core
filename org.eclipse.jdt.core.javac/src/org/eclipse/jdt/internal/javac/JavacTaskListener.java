/*******************************************************************************
* Copyright (c) 2024 Microsoft Corporation and others.
* All rights reserved. This program and the accompanying materials
* are made available under the terms of the Eclipse Public License 2.0
* which accompanies this distribution, and is available at
* https://www.eclipse.org/legal/epl-2.0/
*
* SPDX-License-Identifier: EPL-2.0
*
* Contributors:
*     Microsoft Corporation - initial API and implementation
*******************************************************************************/

package org.eclipse.jdt.internal.javac;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import javax.lang.model.element.TypeElement;
import javax.tools.JavaFileObject;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.ILog;
import org.eclipse.jdt.internal.compiler.ClassFile;
import org.eclipse.jdt.internal.compiler.IProblemFactory;
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit;

import com.sun.source.tree.ClassTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.IdentifierTree;
import com.sun.source.tree.MemberSelectTree;
import com.sun.source.util.TaskEvent;
import com.sun.source.util.TaskListener;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symbol.ClassSymbol;
import com.sun.tools.javac.code.Symbol.PackageSymbol;
import com.sun.tools.javac.code.Symbol.TypeSymbol;
import com.sun.tools.javac.code.Symbol.VarSymbol;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javac.code.Type.ArrayType;
import com.sun.tools.javac.code.Type.MethodType;
import com.sun.tools.javac.code.Type.UnknownType;
import com.sun.tools.javac.tree.JCTree.JCClassDecl;
import com.sun.tools.javac.tree.JCTree.JCFieldAccess;
import com.sun.tools.javac.tree.JCTree.JCIdent;

public class JavacTaskListener implements TaskListener {
	private Map<ICompilationUnit, IContainer> sourceOutputMapping = new HashMap<>();
	private Map<ICompilationUnit, JavacCompilationResult> results = new HashMap<>();
	private UnusedProblemFactory problemFactory;
	private static final Set<String> PRIMITIVE_TYPES = new HashSet<String>(Arrays.asList(
		"byte",
		"short",
		"int",
		"long",
		"float",
		"double",
		"char",
		"boolean"
	));

	public JavacTaskListener(JavacConfig config, Map<IContainer, List<ICompilationUnit>> outputSourceMapping,
			IProblemFactory problemFactory) {
		this.problemFactory = new UnusedProblemFactory(problemFactory, config.compilerOptions());
		for (Entry<IContainer, List<ICompilationUnit>> entry : outputSourceMapping.entrySet()) {
			IContainer currentOutput = entry.getKey();
			entry.getValue().forEach(cu -> sourceOutputMapping.put(cu, currentOutput));
		}
	}

	@Override
	public void finished(TaskEvent e) {
		if (e.getKind() == TaskEvent.Kind.ANALYZE) {
			final JavaFileObject file = e.getSourceFile();
			if (!(file instanceof JavacFileObject)) {
				return;
			}

			final ICompilationUnit cu = ((JavacFileObject) file).getOriginalUnit();
			final JavacCompilationResult result = this.results.computeIfAbsent(cu, (cu1) ->
					new JavacCompilationResult(cu1));
			final Map<Symbol, ClassFile> visitedClasses = new HashMap<Symbol, ClassFile>();
			final Set<ClassSymbol> hierarchyRecorded = new HashSet<>();
			final TypeElement currentTopLevelType = e.getTypeElement();
			UnusedTreeScanner<Void, Void> scanner = new UnusedTreeScanner<>() {
				@Override
				public Void visitClass(ClassTree node, Void p) {
					if (node instanceof JCClassDecl classDecl) {
						/**
						 * If a Java file contains multiple top-level types, it will
						 * trigger multiple ANALYZE taskEvents for the same compilation
						 * unit. Each ANALYZE taskEvent corresponds to the completion
						 * of analysis for a single top-level type. Therefore, in the
						 * ANALYZE task event listener, we only visit the class and nested
						 * classes that belong to the currently analyzed top-level type.
						 */
						if (Objects.equals(currentTopLevelType, classDecl.sym)
							|| !(classDecl.sym.owner instanceof PackageSymbol)) {
							String fullName = classDecl.sym.flatName().toString();
							String compoundName = fullName.replace('.', '/');
							Symbol enclosingClassSymbol = this.getEnclosingClass(classDecl.sym);
							ClassFile enclosingClassFile = enclosingClassSymbol == null ? null : visitedClasses.get(enclosingClassSymbol);
							IContainer expectedOutputDir = sourceOutputMapping.get(cu);
							ClassFile currentClass = new JavacClassFile(fullName, enclosingClassFile, expectedOutputDir);
							visitedClasses.put(classDecl.sym, currentClass);
							result.record(compoundName.toCharArray(), currentClass);
							recordTypeHierarchy(classDecl.sym);
						} else {
							return null; // Skip if it does not belong to the currently analyzed top-level type.
						}
					}

					return super.visitClass(node, p);
				}

				@Override
				public Void visitIdentifier(IdentifierTree node, Void p) {
					if (node instanceof JCIdent id
							&& id.sym instanceof TypeSymbol typeSymbol) {
						String qualifiedName = typeSymbol.getQualifiedName().toString();
						recordQualifiedReference(qualifiedName, false);
					}
					return super.visitIdentifier(node, p);
				}

				@Override
				public Void visitMemberSelect(MemberSelectTree node, Void p) {
					if (node instanceof JCFieldAccess field) {
						if (field.sym != null &&
							!(field.type instanceof MethodType || field.type instanceof UnknownType)) {
							recordQualifiedReference(node.toString(), false);
							if (field.sym instanceof VarSymbol) {
								TypeSymbol elementSymbol = field.type.tsym;
								if (field.type instanceof ArrayType arrayType) {
									elementSymbol = getElementType(arrayType);
								}
								if (elementSymbol instanceof ClassSymbol classSymbol) {
									recordQualifiedReference(classSymbol.className(), true);
								}
							}
						}
					}
					return super.visitMemberSelect(node, p);
				}

				private Symbol getEnclosingClass(Symbol symbol) {
					while (symbol != null) {
						if (symbol.owner instanceof ClassSymbol) {
							return symbol.owner;
						} else if (symbol.owner instanceof PackageSymbol) {
							return null;
						}

						symbol = symbol.owner;
					}

					return null;
				}

				private TypeSymbol getElementType(ArrayType arrayType) {
					if (arrayType.elemtype instanceof ArrayType subArrayType) {
						return getElementType(subArrayType);
					}

					return arrayType.elemtype.tsym;
				}

				private void recordQualifiedReference(String qualifiedName, boolean recursive) {
					if (PRIMITIVE_TYPES.contains(qualifiedName)) {
						return;
					}

					String[] nameParts = qualifiedName.split("\\.");
					int length = nameParts.length;
					if (length == 1) {
						result.addRootReference(nameParts[0]);
						result.addSimpleNameReference(nameParts[0]);
						return;
					}

					if (!recursive) {
						result.addRootReference(nameParts[0]);
						result.addSimpleNameReference(nameParts[length - 1]);
						result.addQualifiedReference(nameParts);
					} else {
						result.addRootReference(nameParts[0]);
						while (result.addQualifiedReference(Arrays.copyOfRange(nameParts, 0, length))) {
							if (length == 2) {
								result.addSimpleNameReference(nameParts[0]);
								result.addSimpleNameReference(nameParts[1]);
								return;
							}

							length--;
							result.addSimpleNameReference(nameParts[length]);
						}
					}
				}

				private void recordTypeHierarchy(ClassSymbol classSymbol) {
					if (hierarchyRecorded.contains(classSymbol)) {
						return;
					}

					hierarchyRecorded.add(classSymbol);
					Type superClass = classSymbol.getSuperclass();
					if (superClass.tsym instanceof ClassSymbol superClassType) {
						recordQualifiedReference(superClassType.className(), true);
						recordTypeHierarchy(superClassType);
					}

					for (Type superInterface : classSymbol.getInterfaces()) {
						if (superInterface.tsym instanceof ClassSymbol superInterfaceType) {
							recordQualifiedReference(superInterfaceType.className(), true);
							recordTypeHierarchy(superInterfaceType);
						}
					}
				}
			};

			final CompilationUnitTree unit = e.getCompilationUnit();
			try {
				scanner.scan(unit, null);
			} catch (Exception ex) {
				ILog.get().error("Internal error when visiting the AST Tree. " + ex.getMessage(), ex);
			}

			result.addUnusedMembers(scanner.getUnusedPrivateMembers(this.problemFactory));
			result.setUnusedImports(scanner.getUnusedImports(this.problemFactory));
		}
	}

	public Map<ICompilationUnit, JavacCompilationResult> getResults() {
		return this.results;
	}
}
