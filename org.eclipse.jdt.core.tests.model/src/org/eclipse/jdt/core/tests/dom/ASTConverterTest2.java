/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.jdt.core.tests.dom;

import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jdt.core.*;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IInitializer;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.ISourceRange;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.compiler.IProblem;
import org.eclipse.jdt.core.dom.*;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.AssertStatement;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ConstructorInvocation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.TypeDeclarationStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.jdt.core.tests.model.ReconcilerTests;

public class ASTConverterTest2 extends ConverterTestSetup {
	
	public ASTConverterTest2(String name) {
		super(name);
	}

	public static Test suite() {
		if (true) {
			return new Suite(ASTConverterTest2.class);		
		}
		TestSuite suite = new Suite(ASTConverterTest2.class.getName());
		suite.addTest(new ASTConverterTest2("test0542"));
		return suite;
	}
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=22560
	 */
	public void test0401() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0401", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0);
		assertNotNull(node);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		Block block = methodDeclaration.getBody();
		List statements = block.statements();
		assertEquals("wrong size", 1, statements.size()); //$NON-NLS-1$
		Statement statement = (Statement) statements.get(0);
		assertTrue("Not a return statement", statement.getNodeType() == ASTNode.RETURN_STATEMENT); //$NON-NLS-1$
		ReturnStatement returnStatement = (ReturnStatement) statement;
		Expression expression = returnStatement.getExpression();
		assertNotNull("there is no expression", expression); //$NON-NLS-1$
		// call the default initialization
		methodDeclaration.getReturnType();
		ITypeBinding typeBinding = expression.resolveTypeBinding();
		assertNotNull("No typebinding", typeBinding); //$NON-NLS-1$
		assertEquals("wrong name", "int", typeBinding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
	}	
	
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=23464
	 */
	public void test0402() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0402", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		ASTNode node = getASTNode((CompilationUnit) result, 1, 0, 0);
		assertEquals("Wrong number of problems", 0, ((CompilationUnit) result).getProblems().length); //$NON-NLS-1$
		assertNotNull(node);
		assertTrue("Not a super method invocation", node.getNodeType() == ASTNode.SUPER_CONSTRUCTOR_INVOCATION); //$NON-NLS-1$
		checkSourceRange(node, "new A().super();", source); //$NON-NLS-1$
	}	
	
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=23597
	 */
	public void test0403() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0403", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		ASTNode node = getASTNode((CompilationUnit) result, 1, 0, 1);
		assertEquals("Wrong number of problems", 1, ((CompilationUnit) result).getProblems().length); //$NON-NLS-1$
		assertNotNull(node);
		assertTrue("Not an expression statement", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertTrue("Not a method invocation", expression.getNodeType() == ASTNode.METHOD_INVOCATION); //$NON-NLS-1$
		MethodInvocation methodInvocation = (MethodInvocation) expression;
		Expression expression2 = methodInvocation.getExpression();
		assertTrue("Not a simple name", expression2.getNodeType() == ASTNode.SIMPLE_NAME); //$NON-NLS-1$
		SimpleName simpleName = (SimpleName) expression2;
		IBinding binding  = simpleName.resolveBinding();
		assertNotNull("No binding", binding); //$NON-NLS-1$
		assertTrue("wrong type", binding.getKind() == IBinding.VARIABLE); //$NON-NLS-1$
		IVariableBinding variableBinding = (IVariableBinding) binding;
		assertEquals("Wrong name", "test", variableBinding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
		SimpleName simpleName2 = methodInvocation.getName();
		assertEquals("Wrong name", "clone", simpleName2.getIdentifier()); //$NON-NLS-1$ //$NON-NLS-2$
		IBinding binding2 = simpleName2.resolveBinding();
		assertNotNull("no binding2", binding2); //$NON-NLS-1$
		assertTrue("Wrong type", binding2.getKind() == IBinding.METHOD); //$NON-NLS-1$
		IMethodBinding methodBinding = (IMethodBinding) binding2;
		assertEquals("Wrong name", "clone", methodBinding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
		IMethodBinding methodBinding2 = methodInvocation.resolveMethodBinding();
		assertNotNull("No method binding2", methodBinding2);
		assertTrue("Wrong binding", methodBinding == methodBinding2);
	}	

	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=23597
	 */
	public void test0404() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0404", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0, 1);
		assertEquals("Wrong number of problems", 1, ((CompilationUnit) result).getProblems().length); //$NON-NLS-1$
		assertNotNull(node);
		assertTrue("Not an expression statement", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertTrue("Not a method invocation", expression.getNodeType() == ASTNode.METHOD_INVOCATION); //$NON-NLS-1$
		MethodInvocation methodInvocation = (MethodInvocation) expression;
		Expression expression2 = methodInvocation.getExpression();
		assertTrue("Not a simple name", expression2.getNodeType() == ASTNode.SIMPLE_NAME); //$NON-NLS-1$
		SimpleName simpleName = (SimpleName) expression2;
		IBinding binding  = simpleName.resolveBinding();
		assertNotNull("No binding", binding); //$NON-NLS-1$
		assertTrue("wrong type", binding.getKind() == IBinding.VARIABLE); //$NON-NLS-1$
		IVariableBinding variableBinding = (IVariableBinding) binding;
		assertEquals("Wrong name", "a", variableBinding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
		SimpleName simpleName2 = methodInvocation.getName();
		assertEquals("Wrong name", "clone", simpleName2.getIdentifier()); //$NON-NLS-1$ //$NON-NLS-2$
		IBinding binding2 = simpleName2.resolveBinding();
		assertNotNull("no binding2", binding2); //$NON-NLS-1$
		assertTrue("Wrong type", binding2.getKind() == IBinding.METHOD); //$NON-NLS-1$
		IMethodBinding methodBinding = (IMethodBinding) binding2;
		assertEquals("Wrong name", "clone", methodBinding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
	}	

	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=23597
	 */
	public void test0405() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0405", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		ASTNode node = getASTNode((CompilationUnit) result, 1, 0, 1);
		assertEquals("Wrong number of problems", 1, ((CompilationUnit) result).getProblems().length); //$NON-NLS-1$
		assertNotNull(node);
		assertTrue("Not an expression statement", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertTrue("Not a method invocation", expression.getNodeType() == ASTNode.METHOD_INVOCATION); //$NON-NLS-1$
		MethodInvocation methodInvocation = (MethodInvocation) expression;
		Expression expression2 = methodInvocation.getExpression();
		assertTrue("Not a simple name", expression2.getNodeType() == ASTNode.SIMPLE_NAME); //$NON-NLS-1$
		SimpleName simpleName = (SimpleName) expression2;
		IBinding binding  = simpleName.resolveBinding();
		assertNotNull("No binding", binding); //$NON-NLS-1$
		assertTrue("wrong type", binding.getKind() == IBinding.VARIABLE); //$NON-NLS-1$
		IVariableBinding variableBinding = (IVariableBinding) binding;
		assertEquals("Wrong name", "a", variableBinding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
		SimpleName simpleName2 = methodInvocation.getName();
		assertEquals("Wrong name", "clone", simpleName2.getIdentifier()); //$NON-NLS-1$ //$NON-NLS-2$
		IBinding binding2 = simpleName2.resolveBinding();
		assertNotNull("no binding2", binding2); //$NON-NLS-1$
		assertTrue("Wrong type", binding2.getKind() == IBinding.METHOD); //$NON-NLS-1$
		IMethodBinding methodBinding = (IMethodBinding) binding2;
		assertEquals("Wrong name", "clone", methodBinding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
	}	

	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=23597
	 */
	public void test0406() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0406", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		CompilationUnit unit = (CompilationUnit) result;
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0, 1);
		assertEquals("Wrong number of problems", 1, ((CompilationUnit) result).getProblems().length); //$NON-NLS-1$
		assertNotNull(node);
		assertTrue("Not an expression statement", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertTrue("Not a method invocation", expression.getNodeType() == ASTNode.METHOD_INVOCATION); //$NON-NLS-1$
		MethodInvocation methodInvocation = (MethodInvocation) expression;
		Expression expression2 = methodInvocation.getExpression();
		assertTrue("Not a simple name", expression2.getNodeType() == ASTNode.SIMPLE_NAME); //$NON-NLS-1$
		SimpleName simpleName = (SimpleName) expression2;
		IBinding binding  = simpleName.resolveBinding();
		assertNotNull("No binding", binding); //$NON-NLS-1$
		assertTrue("wrong type", binding.getKind() == IBinding.VARIABLE); //$NON-NLS-1$
		IVariableBinding variableBinding = (IVariableBinding) binding;
		assertEquals("Wrong name", "a", variableBinding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
		SimpleName simpleName2 = methodInvocation.getName();
		assertEquals("Wrong name", "foo", simpleName2.getIdentifier()); //$NON-NLS-1$ //$NON-NLS-2$
		IBinding binding2 = simpleName2.resolveBinding();
		assertNotNull("no binding2", binding2); //$NON-NLS-1$
		assertTrue("Wrong type", binding2.getKind() == IBinding.METHOD); //$NON-NLS-1$
		IMethodBinding methodBinding = (IMethodBinding) binding2;
		assertEquals("Wrong name", "foo", methodBinding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
		assertNull("Got a declaring node in the unit", unit.findDeclaringNode(methodBinding));
	}	

	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=23162
	 */
	public void test0407() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0407", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertEquals("Wrong number of problems", 0, ((CompilationUnit) result).getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0);
		CompilationUnit unit = (CompilationUnit) result;
		assertNotNull(node);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		SimpleName simpleName = methodDeclaration.getName();
		IBinding binding = simpleName.resolveBinding();
		assertNotNull("No binding", binding); //$NON-NLS-1$
		assertTrue("Not a method binding", binding.getKind() == IBinding.METHOD); //$NON-NLS-1$
		IMethodBinding methodBinding = (IMethodBinding) binding;
		assertEquals("wrong name", "foo", methodBinding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
		methodDeclaration.setName(methodDeclaration.getAST().newSimpleName("foo2")); //$NON-NLS-1$
		IMethodBinding methodBinding2 = methodDeclaration.resolveBinding();
		assertNotNull("No methodbinding2", methodBinding2); //$NON-NLS-1$
		assertEquals("wrong name", "foo", methodBinding2.getName()); //$NON-NLS-1$ //$NON-NLS-2$
		simpleName = methodDeclaration.getName();
		IBinding binding2 = simpleName.resolveBinding();
		assertNull("Got a binding2", binding2); //$NON-NLS-1$
		
		ASTNode astNode = unit.findDeclaringNode(methodBinding);
		assertNotNull("No declaring node", astNode);
		assertEquals("wrong declaring node", methodDeclaration, astNode);
	}
	
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=23162
	 */
	public void test0408() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0408", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertEquals("Wrong number of problems", 0, ((CompilationUnit) result).getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0);
		assertNotNull(node);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		Type type = methodDeclaration.getReturnType();
		assertTrue("Not a simple type", type.isSimpleType()); //$NON-NLS-1$
		SimpleType simpleType = (SimpleType) type;
		Name name = simpleType.getName();
		assertTrue("Not a qualified name", name.isQualifiedName()); //$NON-NLS-1$
		QualifiedName qualifiedName = (QualifiedName) name;
		name = qualifiedName.getQualifier();
		assertTrue("Not a qualified name", name.isQualifiedName()); //$NON-NLS-1$
		qualifiedName = (QualifiedName) name;
		name = qualifiedName.getQualifier();
		assertTrue("Not a simple name", name.isSimpleName()); //$NON-NLS-1$
		SimpleName simpleName = (SimpleName) name;
		IBinding binding = simpleName.resolveBinding();
		assertNotNull("No binding", binding); //$NON-NLS-1$
		assertTrue("Not a package binding", binding.getKind() == IBinding.PACKAGE); //$NON-NLS-1$
		assertEquals("Wrong name", "java", binding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
	}
		
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=23162
	 */
	public void test0409() throws JavaModelException {
		Hashtable options = JavaCore.getOptions();
		Hashtable newOptions = JavaCore.getOptions();
		try {
			newOptions.put(JavaCore.COMPILER_SOURCE, JavaCore.VERSION_1_4);
			JavaCore.setOptions(newOptions);
			ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0409", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			ASTNode result = runConversion(sourceUnit, true);
			assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
			CompilationUnit compilationUnit = (CompilationUnit) result; 
			assertEquals("Wrong number of problems", 1, compilationUnit.getProblems().length); //$NON-NLS-1$
			assertEquals("Unexpected problem", "The import java.lang is never used", compilationUnit.getProblems()[0].getMessage()); //$NON-NLS-1$ //$NON-NLS-2$
			BindingsCollectorVisitor bindingsCollectorVisitor = new BindingsCollectorVisitor();
			compilationUnit.accept(bindingsCollectorVisitor);
			assertEquals("wrong number", 3, bindingsCollectorVisitor.getUnresolvedNodesSet().size()); //$NON-NLS-1$
			Map bindingsMap = bindingsCollectorVisitor.getBindingsMap();
			assertEquals("wrong number", 187, bindingsMap.size()); //$NON-NLS-1$
			ASTNodesCollectorVisitor nodesCollector = new ASTNodesCollectorVisitor();
			compilationUnit.accept(nodesCollector);
			Set detachedNodes = nodesCollector.getDetachedAstNodes();
			int counter = 0;
			for (Iterator iterator = detachedNodes.iterator(); iterator.hasNext(); ) {
				ASTNode detachedNode = (ASTNode) iterator.next();
				counter++;
				IBinding binding = (IBinding) bindingsMap.get(detachedNode);
				assertNotNull(binding);
				switch(detachedNode.getNodeType()) {
					case ASTNode.ARRAY_ACCESS :
					case ASTNode.ARRAY_CREATION :
					case ASTNode.ARRAY_INITIALIZER :
					case ASTNode.ASSIGNMENT :
					case ASTNode.BOOLEAN_LITERAL :
					case ASTNode.CAST_EXPRESSION :
					case ASTNode.CHARACTER_LITERAL :
					case ASTNode.CLASS_INSTANCE_CREATION :
					case ASTNode.CONDITIONAL_EXPRESSION :
					case ASTNode.FIELD_ACCESS :
					case ASTNode.INFIX_EXPRESSION :
					case ASTNode.INSTANCEOF_EXPRESSION :
					case ASTNode.METHOD_INVOCATION :
					case ASTNode.NULL_LITERAL :
					case ASTNode.NUMBER_LITERAL :
					case ASTNode.POSTFIX_EXPRESSION :
					case ASTNode.PREFIX_EXPRESSION :
					case ASTNode.THIS_EXPRESSION :
					case ASTNode.TYPE_LITERAL :
					case ASTNode.VARIABLE_DECLARATION_EXPRESSION :
						ITypeBinding typeBinding = ((Expression) detachedNode).resolveTypeBinding();
						if (!binding.equals(typeBinding)) {
							System.out.println(detachedNode);
						}
						assertTrue("binding not equals", binding.equals(typeBinding)); //$NON-NLS-1$
						break;						
					case ASTNode.VARIABLE_DECLARATION_FRAGMENT :
						assertTrue("binding not equals", binding.equals(((VariableDeclarationFragment) detachedNode).resolveBinding())); //$NON-NLS-1$
						break;						
					case ASTNode.ANONYMOUS_CLASS_DECLARATION :
						assertTrue("binding not equals", binding.equals(((AnonymousClassDeclaration) detachedNode).resolveBinding())); //$NON-NLS-1$
						break;
					case ASTNode.QUALIFIED_NAME :
					case ASTNode.SIMPLE_NAME :
						IBinding newBinding = ((Name) detachedNode).resolveBinding();
						assertTrue("binding not equals", binding.equals(newBinding)); //$NON-NLS-1$
						break;
					case ASTNode.ARRAY_TYPE :
					case ASTNode.SIMPLE_TYPE :
					case ASTNode.PRIMITIVE_TYPE :
						assertTrue("binding not equals", binding.equals(((Type) detachedNode).resolveBinding())); //$NON-NLS-1$
						break;
					case ASTNode.CONSTRUCTOR_INVOCATION :
						assertTrue("binding not equals", binding.equals(((ConstructorInvocation) detachedNode).resolveConstructorBinding())); //$NON-NLS-1$
						break;
					case ASTNode.IMPORT_DECLARATION :
						assertTrue("binding not equals", binding.equals(((ImportDeclaration) detachedNode).resolveBinding())); //$NON-NLS-1$
						break;
					case ASTNode.METHOD_DECLARATION :
						assertTrue("binding not equals", binding.equals(((MethodDeclaration) detachedNode).resolveBinding())); //$NON-NLS-1$
						break;
					case ASTNode.PACKAGE_DECLARATION :
						assertTrue("binding not equals", binding.equals(((PackageDeclaration) detachedNode).resolveBinding())); //$NON-NLS-1$
						break;
					case ASTNode.TYPE_DECLARATION :
						assertTrue("binding not equals", binding.equals(((TypeDeclaration) detachedNode).resolveBinding())); //$NON-NLS-1$
						break;
				}
			}
		} finally {
			JavaCore.setOptions(options);
		}
	}

	/**
	 * Test for message on jdt-core-dev
	 */
	public void test0410() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0410", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertEquals("Wrong number of problems", 0, ((CompilationUnit) result).getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0, 0);
		assertNotNull(node);
		assertTrue("Not a return statement", node.getNodeType() == ASTNode.RETURN_STATEMENT); //$NON-NLS-1$
		Expression expression = ((ReturnStatement) node).getExpression();
		assertTrue("Not an infix expression", expression.getNodeType() == ASTNode.INFIX_EXPRESSION); //$NON-NLS-1$
		InfixExpression infixExpression = (InfixExpression) expression;
		List extendedOperands = infixExpression.extendedOperands();
		assertEquals("wrong size", 3, extendedOperands.size()); //$NON-NLS-1$
	}

	/**
	 * Test for message on jdt-core-dev
	 */
	public void test0411() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0411", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertEquals("Wrong number of problems", 0, ((CompilationUnit) result).getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0, 0);
		assertNotNull(node);
		assertTrue("Not a return statement", node.getNodeType() == ASTNode.RETURN_STATEMENT); //$NON-NLS-1$
		Expression expression = ((ReturnStatement) node).getExpression();
		assertTrue("Not an infix expression", expression.getNodeType() == ASTNode.INFIX_EXPRESSION); //$NON-NLS-1$
		InfixExpression infixExpression = (InfixExpression) expression;
		List extendedOperands = infixExpression.extendedOperands();
		assertEquals("wrong size", 0, extendedOperands.size()); //$NON-NLS-1$
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=23901
	 */
	public void test0412() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0412", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0);
		assertNotNull(node);
		assertTrue("Not a type declaration", node.getNodeType() == ASTNode.TYPE_DECLARATION); //$NON-NLS-1$
		TypeDeclaration typeDeclaration = (TypeDeclaration) node;
		assertTrue("Not an interface", typeDeclaration.isInterface()); //$NON-NLS-1$
		ITypeBinding typeBinding = typeDeclaration.resolveBinding();
		assertNotNull("No type binding", typeBinding); //$NON-NLS-1$
		assertNotNull("No declaring node", unit.findDeclaringNode(typeBinding)); //$NON-NLS-1$
		Name name = typeDeclaration.getName();
		IBinding binding = name.resolveBinding();
		assertNotNull("No binding", binding); //$NON-NLS-1$
		ASTNode declaringNode = unit.findDeclaringNode(binding);
		assertNotNull("No declaring node", declaringNode); //$NON-NLS-1$
		assertEquals("Wrong node", typeDeclaration, declaringNode); //$NON-NLS-1$
		typeBinding = name.resolveTypeBinding();
		assertNotNull("No type binding", typeBinding); //$NON-NLS-1$
		declaringNode = unit.findDeclaringNode(typeBinding);
		assertNotNull("No declaring node", declaringNode); //$NON-NLS-1$
		assertEquals("Wrong node", typeDeclaration, declaringNode); //$NON-NLS-1$
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=20881
	 */
	public void test0413() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0413", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 1, 0);
		assertNotNull(node);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		List throwsException = methodDeclaration.thrownExceptions();
		assertEquals("wrong size", 2, throwsException.size()); //$NON-NLS-1$
		Name name = (Name) throwsException.get(0);
		IBinding binding = name.resolveBinding();
		assertNull("Got a binding", binding); //$NON-NLS-1$
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=23734
	 */
	public void test0414() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0414", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0, 0);
		assertNotNull(node);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		Type type = methodDeclaration.getReturnType();
		ITypeBinding typeBinding = type.resolveBinding();
		assertNotNull("No type binding", typeBinding); //$NON-NLS-1$
		ASTNode declaringNode = unit.findDeclaringNode(typeBinding);
		assertNull("Got a declaring node", declaringNode); //$NON-NLS-1$

		node = getASTNode(unit, 0, 1);
		assertNotNull(node);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration2 = (MethodDeclaration) node;
		Type type2 = methodDeclaration2.getReturnType();
		ITypeBinding typeBinding2 = type2.resolveBinding();
		assertNotNull("No type binding", typeBinding2); //$NON-NLS-1$
		ASTNode declaringNode2 = unit.findDeclaringNode(typeBinding2);
		assertNotNull("No declaring node", declaringNode2); //$NON-NLS-1$

		ICompilationUnit sourceUnit2 = getCompilationUnit("Converter" , "src", "test0414", "B.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		
		result = runConversion(sourceUnit2, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit2 = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit2.getProblems().length); //$NON-NLS-1$
		ASTNode declaringNode3 = unit2.findDeclaringNode(typeBinding);
		assertNull("Got a declaring node", declaringNode3); //$NON-NLS-1$
		
		ASTNode declaringNode4 = unit2.findDeclaringNode(typeBinding.getKey());
		assertNotNull("No declaring node", declaringNode4); //$NON-NLS-1$
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24268
	 */
	public void test0415() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0415", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a switch statement", node.getNodeType() == ASTNode.SWITCH_STATEMENT); //$NON-NLS-1$
		SwitchStatement switchStatement = (SwitchStatement) node;
		List statements = switchStatement.statements();
		assertEquals("wrong size", statements.size(), 5); //$NON-NLS-1$
		Statement statement = (Statement) statements.get(3);
		assertTrue("not a switch case (default)", statement.getNodeType() == ASTNode.SWITCH_CASE); //$NON-NLS-1$
		SwitchCase defaultCase = (SwitchCase) statement;
		assertTrue("not a default case", defaultCase.isDefault());
		checkSourceRange(defaultCase, "default:", source);
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24324
	 */
	public void test0416() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0416", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 1, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a variable declaration statement", node.getNodeType() == ASTNode.VARIABLE_DECLARATION_STATEMENT); //$NON-NLS-1$
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		List fragments = statement.fragments();
		assertEquals("Wrong size", fragments.size(), 1);
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression init = fragment.getInitializer();
		assertTrue("not a qualified name", init.getNodeType() == ASTNode.QUALIFIED_NAME); //$NON-NLS-1$
		QualifiedName qualifiedName = (QualifiedName) init;
		SimpleName simpleName = qualifiedName.getName();
		assertEquals("Wrong name", "CONST", simpleName.getIdentifier());
		IBinding binding = simpleName.resolveBinding();
		assertEquals("Wrong type", IBinding.VARIABLE, binding.getKind());
		IVariableBinding variableBinding = (IVariableBinding) binding;
		assertEquals("Wrong modifier", variableBinding.getModifiers(), Modifier.PUBLIC | Modifier.STATIC | Modifier.FINAL);
		ASTNode declaringNode = unit.findDeclaringNode(variableBinding);
		assertNotNull("No declaring node", declaringNode);
		assertTrue("not a variable declaration fragment", declaringNode.getNodeType() == ASTNode.VARIABLE_DECLARATION_FRAGMENT);
		VariableDeclarationFragment variableDeclarationFragment = (VariableDeclarationFragment) declaringNode;
		FieldDeclaration fieldDeclaration = (FieldDeclaration) variableDeclarationFragment.getParent();
		assertEquals("Wrong modifier", fieldDeclaration.getModifiers(), Modifier.NONE);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24347
	 */
	public void test0417() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0417", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a variable declaration statement", node.getNodeType() == ASTNode.VARIABLE_DECLARATION_STATEMENT); //$NON-NLS-1$
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		Type type = statement.getType();
		assertTrue("not a simple type", type.getNodeType() == ASTNode.SIMPLE_TYPE); //$NON-NLS-1$
		SimpleType simpleType = (SimpleType) type;
		Name name = simpleType.getName();
		assertTrue("Not a qualified name", name.isQualifiedName());
		QualifiedName qualifiedName = (QualifiedName) name;
		Name qualifier = qualifiedName.getQualifier();
		assertTrue("Not a simple name", qualifier.isSimpleName());
		IBinding binding = qualifier.resolveBinding();
		assertNotNull("No binding", binding);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24406
	 */
	public void test0418() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0418", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 1, 0);
		assertNotNull("No node", node);
		assertTrue("not an expression statement ", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertTrue("not an method invocation", expression.getNodeType() == ASTNode.METHOD_INVOCATION); //$NON-NLS-1$
		MethodInvocation methodInvocation = (MethodInvocation) expression;
		Name name = methodInvocation.getName();
		IBinding binding = name.resolveBinding();
		assertNotNull("No binding", binding);
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24449
	 */
	public void test0419() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0419", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 1, 0, 0);
		assertEquals("Not an expression statement", node.getNodeType(), ASTNode.EXPRESSION_STATEMENT);
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertEquals("Not an assignment", expression.getNodeType(), ASTNode.ASSIGNMENT);
		Assignment assignment = (Assignment) expression;
		Expression expression2 = assignment.getLeftHandSide();
		assertEquals("Not a name", expression2.getNodeType(), ASTNode.SIMPLE_NAME);
		SimpleName simpleName = (SimpleName) expression2;
		IBinding binding = simpleName.resolveBinding();
		assertNull(binding);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24453
	 */
	public void test0420() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0420", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Not a variable declaration statement", node.getNodeType(), ASTNode.VARIABLE_DECLARATION_STATEMENT);
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		List fragments = statement.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertEquals("Not an infix expression", expression.getNodeType(), ASTNode.INFIX_EXPRESSION);
		InfixExpression infixExpression = (InfixExpression) expression;
		Expression expression2 = infixExpression.getRightOperand();
		assertEquals("Not a parenthesized expression", expression2.getNodeType(), ASTNode.PARENTHESIZED_EXPRESSION);
		checkSourceRange(expression2, "(2 + 3)", source);
		ParenthesizedExpression parenthesizedExpression = (ParenthesizedExpression) expression2;
		Expression expression3 = parenthesizedExpression.getExpression();
		checkSourceRange(expression3, "2 + 3", source);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24453
	 */
	public void test0421() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0421", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Not a variable declaration statement", node.getNodeType(), ASTNode.VARIABLE_DECLARATION_STATEMENT);
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		List fragments = statement.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertEquals("Not an infix expression", expression.getNodeType(), ASTNode.INFIX_EXPRESSION);
		InfixExpression infixExpression = (InfixExpression) expression;
		checkSourceRange(infixExpression, "(1 + 2) + 3", source);
		Expression expression2 = infixExpression.getLeftOperand();
		assertEquals("Not a parenthesized expression", expression2.getNodeType(), ASTNode.PARENTHESIZED_EXPRESSION);
		checkSourceRange(expression2, "(1 + 2)", source);
		ParenthesizedExpression parenthesizedExpression = (ParenthesizedExpression) expression2;
		Expression expression3 = parenthesizedExpression.getExpression();
		checkSourceRange(expression3, "1 + 2", source);
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24453
	 */
	public void test0422() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0422", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Not a variable declaration statement", node.getNodeType(), ASTNode.VARIABLE_DECLARATION_STATEMENT);
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		List fragments = statement.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertEquals("Not an infix expression", expression.getNodeType(), ASTNode.INFIX_EXPRESSION);
		InfixExpression infixExpression = (InfixExpression) expression;
		checkSourceRange(infixExpression, "( 1 + 2 ) + 3", source);
		Expression expression2 = infixExpression.getLeftOperand();
		assertEquals("Not a parenthesized expression", expression2.getNodeType(), ASTNode.PARENTHESIZED_EXPRESSION);
		checkSourceRange(expression2, "( 1 + 2 )", source);
		ParenthesizedExpression parenthesizedExpression = (ParenthesizedExpression) expression2;
		Expression expression3 = parenthesizedExpression.getExpression();
		checkSourceRange(expression3, "1 + 2", source);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24453
	 */
	public void test0423() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0423", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Not a variable declaration statement", node.getNodeType(), ASTNode.VARIABLE_DECLARATION_STATEMENT);
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		List fragments = statement.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertEquals("Not an infix expression", expression.getNodeType(), ASTNode.INFIX_EXPRESSION);
		InfixExpression infixExpression = (InfixExpression) expression;
		Expression expression2 = infixExpression.getRightOperand();
		assertEquals("Not a parenthesized expression", expression2.getNodeType(), ASTNode.PARENTHESIZED_EXPRESSION);
		checkSourceRange(expression2, "( 2 + 3 )", source);
		ParenthesizedExpression parenthesizedExpression = (ParenthesizedExpression) expression2;
		Expression expression3 = parenthesizedExpression.getExpression();
		checkSourceRange(expression3, "2 + 3", source);
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24453
	 */
	public void test0424() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0424", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Not a variable declaration statement", node.getNodeType(), ASTNode.VARIABLE_DECLARATION_STATEMENT);
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		List fragments = statement.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertEquals("Not an infix expression", expression.getNodeType(), ASTNode.INFIX_EXPRESSION);
		InfixExpression infixExpression = (InfixExpression) expression;
		assertEquals("Wrong size", 1, infixExpression.extendedOperands().size());
		Expression expression2 = (Expression) infixExpression.extendedOperands().get(0);
		checkSourceRange(expression2, "( 2 + 3 )", source);
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24453
	 */
	public void test0425() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0425", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Not a variable declaration statement", node.getNodeType(), ASTNode.VARIABLE_DECLARATION_STATEMENT);
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		List fragments = statement.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertEquals("Not an infix expression", expression.getNodeType(), ASTNode.INFIX_EXPRESSION);
		InfixExpression infixExpression = (InfixExpression) expression;
		assertEquals("Wrong size", 0, infixExpression.extendedOperands().size());
		Expression expression2 = infixExpression.getRightOperand();
		assertTrue("not an infix expression", expression2.getNodeType() == ASTNode.INFIX_EXPRESSION); //$NON-NLS-1$
		InfixExpression infixExpression2 = (InfixExpression) expression2;
		Expression expression3 = infixExpression2.getRightOperand();
		assertTrue("not a parenthesized expression", expression3.getNodeType() == ASTNode.PARENTHESIZED_EXPRESSION); //$NON-NLS-1$
		checkSourceRange(expression3, "( 2 + 3 )", source);
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24449
	 */
	public void test0426() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0426", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 1, 0, 0);
		assertEquals("Not a variable declaration statement", node.getNodeType(), ASTNode.VARIABLE_DECLARATION_STATEMENT);
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		Type type = statement.getType();
		assertTrue("not a simple type", type.getNodeType() == ASTNode.SIMPLE_TYPE);
		SimpleType simpleType = (SimpleType) type;
		Name name = simpleType.getName();
		assertNotNull("No name", name);
		IBinding binding = name.resolveBinding();
		assertNotNull("No binding", binding);
	}	
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24449
	 */
	public void test0427() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0427", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 1, 0, 0);
		assertEquals("Not an expression statement", node.getNodeType(), ASTNode.EXPRESSION_STATEMENT);
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertEquals("Not an assignment", expression.getNodeType(), ASTNode.ASSIGNMENT);
		Assignment assignment = (Assignment) expression;
		Expression expression2 = assignment.getLeftHandSide();
		assertEquals("Not a super field access", expression2.getNodeType(), ASTNode.SUPER_FIELD_ACCESS);
		SuperFieldAccess superFieldAccess = (SuperFieldAccess) expression2;
		Name name = superFieldAccess.getName();
		assertNotNull("No name", name);
		IBinding binding = name.resolveBinding();
		assertNull("Got a binding", binding);
		assertNull("Got a binding", superFieldAccess.resolveFieldBinding());
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24449
	 */
	public void test0428() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0428", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 1, 0, 0);
		assertEquals("Not an expression statement", node.getNodeType(), ASTNode.EXPRESSION_STATEMENT);
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertEquals("Not an assignment", expression.getNodeType(), ASTNode.ASSIGNMENT);
		Assignment assignment = (Assignment) expression;
		Expression expression2 = assignment.getLeftHandSide();
		assertEquals("Not a qualified name", expression2.getNodeType(), ASTNode.QUALIFIED_NAME);
		QualifiedName name = (QualifiedName) expression2;
		SimpleName simpleName = name.getName();
		IBinding binding = simpleName.resolveBinding();
		assertNotNull("No binding", binding);
		IBinding binding2 = name.resolveBinding();
		assertNotNull("No binding2", binding2);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24449
	 */
	public void test0429() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0429", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 1, 0, 0);
		assertEquals("Not an expression statement", node.getNodeType(), ASTNode.EXPRESSION_STATEMENT);
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertEquals("Not an assignment", expression.getNodeType(), ASTNode.ASSIGNMENT);
		Assignment assignment = (Assignment) expression;
		Expression expression2 = assignment.getLeftHandSide();
		assertEquals("Not a qualified name", expression2.getNodeType(), ASTNode.QUALIFIED_NAME);
		QualifiedName name = (QualifiedName) expression2;
		SimpleName simpleName = name.getName();
		IBinding binding = simpleName.resolveBinding();
		assertNotNull("No binding", binding);
		IBinding binding2 = name.resolveBinding();
		assertNotNull("No binding2", binding2);
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24499
	 */
	public void test0430() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0430", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertTrue("Not a constructor invocation", node.getNodeType() == ASTNode.CONSTRUCTOR_INVOCATION);
		ConstructorInvocation constructorInvocation = (ConstructorInvocation) node;
		checkSourceRange(constructorInvocation, "this(coo2());", source);
		List arguments = constructorInvocation.arguments();
		assertEquals("Wrong size", 1, arguments.size());
		Expression expression = (Expression) arguments.get(0);
		assertTrue("Not a method invocation", expression.getNodeType() == ASTNode.METHOD_INVOCATION);
		MethodInvocation methodInvocation = (MethodInvocation) expression;
		SimpleName simpleName = methodInvocation.getName();
		IBinding binding = simpleName.resolveBinding();
		assertNotNull("No binding", binding);
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24500
	 */
	public void test0431() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0431", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertTrue("Not a constructor invocation", node.getNodeType() == ASTNode.CONSTRUCTOR_INVOCATION);
		ConstructorInvocation constructorInvocation = (ConstructorInvocation) node;
		List arguments = constructorInvocation.arguments();
		assertEquals("Wrong size", 1, arguments.size());
		Expression expression = (Expression) arguments.get(0);
		assertTrue("Not a simple name", expression.getNodeType() == ASTNode.SIMPLE_NAME);
		SimpleName simpleName = (SimpleName) expression;
		IBinding binding = simpleName.resolveBinding();
		assertNotNull("No binding", binding);
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24501
	 */
	public void test0432() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0432", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 1, 0, 0);
		assertEquals("Not an expression statement", ASTNode.EXPRESSION_STATEMENT, node.getNodeType());
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertEquals("Not an assignment", ASTNode.ASSIGNMENT, expression.getNodeType());
		Assignment assignment = (Assignment) expression;
		Expression expression2 = assignment.getLeftHandSide();
		assertEquals("Not a simple name", ASTNode.SIMPLE_NAME, expression2.getNodeType());
		SimpleName simpleName = (SimpleName) expression2;
		IBinding binding = simpleName.resolveBinding();
		assertNotNull("No binding", binding);
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24501
	 */
	public void test0433() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0433", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 1, 0, 0);
		assertEquals("Not an expression statement", ASTNode.EXPRESSION_STATEMENT, node.getNodeType());
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertEquals("Not an assignment", ASTNode.ASSIGNMENT, expression.getNodeType());
		Assignment assignment = (Assignment) expression;
		Expression expression2 = assignment.getLeftHandSide();
		assertEquals("Not a super field access", ASTNode.SUPER_FIELD_ACCESS, expression2.getNodeType());
		SuperFieldAccess superFieldAccess = (SuperFieldAccess) expression2;
		SimpleName simpleName = superFieldAccess.getName();
		assertEquals("wrong name", "fCoo", simpleName.getIdentifier());
		IBinding binding = simpleName.resolveBinding();
		assertNotNull("No binding", binding);
		assertEquals("Wrong binding", IBinding.VARIABLE, binding.getKind());
		IVariableBinding variableBinding = superFieldAccess.resolveFieldBinding();
		assertTrue("Different binding", binding == variableBinding);
		ASTNode astNode = unit.findDeclaringNode(variableBinding);
		assertTrue("Wrong type", astNode.getNodeType() == ASTNode.SINGLE_VARIABLE_DECLARATION || astNode.getNodeType() == ASTNode.VARIABLE_DECLARATION_FRAGMENT || astNode.getNodeType() == ASTNode.VARIABLE_DECLARATION_EXPRESSION);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24501
	 */
	public void test0434() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0434", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 1, 0, 0);
		assertEquals("Not an expression statement", ASTNode.EXPRESSION_STATEMENT, node.getNodeType());
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertEquals("Not an assignment", ASTNode.ASSIGNMENT, expression.getNodeType());
		Assignment assignment = (Assignment) expression;
		Expression expression2 = assignment.getLeftHandSide();
		assertEquals("Not a qualified name", ASTNode.QUALIFIED_NAME, expression2.getNodeType());
		QualifiedName qualifiedName = (QualifiedName) expression2;
		SimpleName simpleName = qualifiedName.getName();
		assertEquals("wrong name", "fCoo", simpleName.getIdentifier());
		IBinding binding = simpleName.resolveBinding();
		assertNotNull("No binding", binding);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24501
	 */
	public void test0435() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0435", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 1, 0, 0);
		assertEquals("Not an expression statement", ASTNode.EXPRESSION_STATEMENT, node.getNodeType());
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertEquals("Not an assignment", ASTNode.ASSIGNMENT, expression.getNodeType());
		Assignment assignment = (Assignment) expression;
		Expression expression2 = assignment.getLeftHandSide();
		assertEquals("Not a qualified name", ASTNode.QUALIFIED_NAME, expression2.getNodeType());
		QualifiedName qualifiedName = (QualifiedName) expression2;
		SimpleName simpleName = qualifiedName.getName();
		assertEquals("wrong name", "fCoo", simpleName.getIdentifier());
		IBinding binding = simpleName.resolveBinding();
		assertNotNull("No binding", binding);
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24502
	 */
	public void test0436() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0436", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 1, 0, 0);
		assertEquals("Not a variable declaration statement", ASTNode.VARIABLE_DECLARATION_STATEMENT, node.getNodeType());
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		Type type = statement.getType();
		assertEquals("Not a simple type", ASTNode.SIMPLE_TYPE, type.getNodeType());
		SimpleType simpleType = (SimpleType) type;
		Name name = simpleType.getName();
		IBinding binding = name.resolveBinding();
		assertNull("Got a binding", binding);
		assertEquals("Not a qualified name", ASTNode.QUALIFIED_NAME, name.getNodeType());
		QualifiedName qualifiedName = (QualifiedName) name;
		SimpleName simpleName = qualifiedName.getName();
		assertEquals("wrong name", "CInner", simpleName.getIdentifier());
		IBinding binding2 = simpleName.resolveBinding();
		assertNull("Got a binding", binding2);
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24502
	 */
	public void test0437() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0437", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 1, 0, 0);
		assertEquals("Not a variable declaration statement", ASTNode.VARIABLE_DECLARATION_STATEMENT, node.getNodeType());
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		Type type = statement.getType();
		assertEquals("Not a simple type", ASTNode.SIMPLE_TYPE, type.getNodeType());
		SimpleType simpleType = (SimpleType) type;
		Name name = simpleType.getName();
		assertEquals("Not a simple name", ASTNode.SIMPLE_NAME, name.getNodeType());
		SimpleName simpleName = (SimpleName) name;
		IBinding binding = simpleName.resolveBinding();
		assertNull("No binding", binding);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24511
	 */
	public void test0438() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0438", "D.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		List imports = unit.imports();
		assertEquals("Wrong size", 1, imports.size()); //$NON-NLS-1$<
		ImportDeclaration importDeclaration = (ImportDeclaration) imports.get(0);
		IBinding binding = importDeclaration.resolveBinding();
		assertNotNull("No binding", binding);
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24502
	 */
	public void test0439() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0439", "E.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Not a variable declaration statement", ASTNode.VARIABLE_DECLARATION_STATEMENT, node.getNodeType());
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		Type type = statement.getType();
		assertEquals("Not a simple type", ASTNode.SIMPLE_TYPE, type.getNodeType());
		SimpleType simpleType = (SimpleType) type;
		Name name = simpleType.getName();
		IBinding binding = name.resolveBinding();
		assertNotNull("No binding", binding);
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24622
	 */
	public void test0440() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0440", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Not a variable declaration statement", ASTNode.VARIABLE_DECLARATION_STATEMENT, node.getNodeType());
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		List fragments = statement.fragments();
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertEquals("Not an infix expression", ASTNode.INFIX_EXPRESSION, expression.getNodeType());
		// 2 * 3 + "" + (true)
		InfixExpression infixExpression = (InfixExpression) expression;
		checkSourceRange(infixExpression, "2 * 3 + \"\" + (true)", source);
		Expression leftOperand = infixExpression.getLeftOperand();
		checkSourceRange(leftOperand, "2 * 3 + \"\"", source);
		checkSourceRange(infixExpression.getRightOperand(), "(true)", source);
		assertEquals("wrong operator", infixExpression.getOperator(), InfixExpression.Operator.PLUS);
		assertEquals("wrong type", ASTNode.INFIX_EXPRESSION, leftOperand.getNodeType());
		infixExpression = (InfixExpression) leftOperand;
		checkSourceRange(infixExpression, "2 * 3 + \"\"", source);
		leftOperand = infixExpression.getLeftOperand();
		checkSourceRange(leftOperand, "2 * 3", source);
		checkSourceRange(infixExpression.getRightOperand(), "\"\"", source);
		assertEquals("wrong operator", infixExpression.getOperator(), InfixExpression.Operator.PLUS);
		assertEquals("wrong type", ASTNode.INFIX_EXPRESSION, leftOperand.getNodeType());
		infixExpression = (InfixExpression) leftOperand;
		checkSourceRange(infixExpression, "2 * 3", source);
		leftOperand = infixExpression.getLeftOperand();
		checkSourceRange(leftOperand, "2", source);
		checkSourceRange(infixExpression.getRightOperand(), "3", source);
		assertEquals("wrong operator", infixExpression.getOperator(), InfixExpression.Operator.TIMES);
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24622
	 */
	public void test0441() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0441", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Not a variable declaration statement", ASTNode.VARIABLE_DECLARATION_STATEMENT, node.getNodeType());
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		List fragments = statement.fragments();
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertEquals("Not an infix expression", ASTNode.INFIX_EXPRESSION, expression.getNodeType());
		InfixExpression infixExpression = (InfixExpression) expression;
		checkSourceRange(infixExpression, "(2 + 2) * 3 * 1", source);
		Expression leftOperand = infixExpression.getLeftOperand();
		checkSourceRange(leftOperand, "(2 + 2)", source);
		checkSourceRange(infixExpression.getRightOperand(), "3", source);
		List extendedOperands = infixExpression.extendedOperands();
		assertEquals("wrong size", 1, extendedOperands.size());
		checkSourceRange((Expression) extendedOperands.get(0), "1", source);
		assertEquals("wrong operator", InfixExpression.Operator.TIMES, infixExpression.getOperator());
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24622
	 */
	public void test0442() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0442", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Not a variable declaration statement", ASTNode.VARIABLE_DECLARATION_STATEMENT, node.getNodeType());
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		List fragments = statement.fragments();
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertEquals("Not an infix expression", ASTNode.INFIX_EXPRESSION, expression.getNodeType());
		InfixExpression infixExpression = (InfixExpression) expression;
		checkSourceRange(infixExpression, "2 + (2 * 3) + 1", source);
		Expression leftOperand = infixExpression.getLeftOperand();
		checkSourceRange(leftOperand, "2", source);
		Expression rightOperand = infixExpression.getRightOperand();
		checkSourceRange(rightOperand, "(2 * 3)", source);
		assertEquals("wrong type", ASTNode.PARENTHESIZED_EXPRESSION, rightOperand.getNodeType());
		List extendedOperands = infixExpression.extendedOperands();
		assertEquals("wrong size", 1, extendedOperands.size());
		checkSourceRange((Expression) extendedOperands.get(0), "1", source);
		assertEquals("wrong operator", InfixExpression.Operator.PLUS, infixExpression.getOperator());
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24623
	 */
	public void test0443() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0443", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 2, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0);
		assertEquals("Wrong type", ASTNode.METHOD_DECLARATION, node.getNodeType());
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertNotNull("No body", methodDeclaration.getBody());
		assertNotNull("No binding", methodDeclaration.resolveBinding());
		assertTrue("Not an abstract method", Modifier.isAbstract(methodDeclaration.getModifiers())); 
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24623
	 */
	public void test0444() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0444", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 2, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0);
		assertEquals("Wrong type", ASTNode.TYPE_DECLARATION, node.getNodeType());
		TypeDeclaration typeDeclaration = (TypeDeclaration) node;
		List bodyDeclarations = typeDeclaration.bodyDeclarations();
		assertEquals("Wrong size", 2, bodyDeclarations.size());
		BodyDeclaration bodyDeclaration = (BodyDeclaration)bodyDeclarations.get(0);
		assertEquals("Wrong type", ASTNode.METHOD_DECLARATION, bodyDeclaration.getNodeType());
		MethodDeclaration methodDeclaration = (MethodDeclaration) bodyDeclaration;
		assertEquals("Wrong name", "foo", methodDeclaration.getName().getIdentifier());
		assertNull("Got a binding", methodDeclaration.resolveBinding());
		bodyDeclaration = (BodyDeclaration)bodyDeclarations.get(1);
		assertEquals("Wrong type", ASTNode.METHOD_DECLARATION, bodyDeclaration.getNodeType());
		assertEquals("Wrong name", "foo", ((MethodDeclaration) bodyDeclaration).getName().getIdentifier());
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24773
	 */
	public void test0445() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0445", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=25018
	 */
	public void test0446() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0446", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=25124
	 */
	public void test0447() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0447", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 3, unit.getProblems().length); //$NON-NLS-1$<
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=25330
	 */
	public void test0448() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0448", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0);
		assertEquals("Not a method declaration", node.getNodeType(), ASTNode.METHOD_DECLARATION);
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertTrue("Not a constructor", methodDeclaration.isConstructor());
		ITypeBinding returnTypeBinding = methodDeclaration.getReturnType().resolveBinding();
		assertNotNull("No return type binding", returnTypeBinding);
		Block block = methodDeclaration.getBody();
		assertNotNull("No method body", block);
		assertEquals("wrong size", 0, block.statements().size()); 
	}
	
	/**
	 * Check that the implicit super constructor call is not there
	 */
	public void test0449() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0449", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0);
		assertEquals("Not a method declaration", node.getNodeType(), ASTNode.METHOD_DECLARATION);
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertTrue("Not a constructor", methodDeclaration.isConstructor());
		Block block = methodDeclaration.getBody();
		assertNotNull("No method body", block);
		assertEquals("wrong size", 1, block.statements().size()); 
	}	
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=26452
	 */
	public void test0450() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0450", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0);
		assertEquals("Not a type declaration", node.getNodeType(), ASTNode.TYPE_DECLARATION);
		TypeDeclaration typeDeclaration = (TypeDeclaration) node;
		ITypeBinding typeBinding = typeDeclaration.resolveBinding();
		assertNotNull("No type binding", typeBinding);
		assertTrue("not a class", typeBinding.isClass());
		assertTrue("not a toplevel type", typeBinding.isTopLevel());
		assertTrue("a local type", !typeBinding.isLocal());
		assertTrue("an anonymous type", !typeBinding.isAnonymous());
		assertTrue("a member type", !typeBinding.isMember());
		assertTrue("a nested type", !typeBinding.isNested());
		node = getASTNode(unit, 0, 0, 0);
		assertEquals("Not an expression statement", node.getNodeType(), ASTNode.EXPRESSION_STATEMENT);
		Expression expression = ((ExpressionStatement) node).getExpression();
		assertEquals("Not a class instance creation", expression.getNodeType(), ASTNode.CLASS_INSTANCE_CREATION);
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expression;
		AnonymousClassDeclaration anonymousClassDeclaration = classInstanceCreation.getAnonymousClassDeclaration();
		typeBinding = anonymousClassDeclaration.resolveBinding();
		assertNotNull("No type binding", typeBinding);
		assertTrue("not a class", typeBinding.isClass());
		assertTrue("a toplevel type", !typeBinding.isTopLevel());
		assertTrue("not a local type", typeBinding.isLocal());
		assertTrue("not an anonymous type", typeBinding.isAnonymous());
		assertTrue("a member type", !typeBinding.isMember());
		assertTrue("not a nested type", typeBinding.isNested());
		ASTNode astNode = unit.findDeclaringNode(typeBinding);
		assertEquals("Wrong type", ASTNode.ANONYMOUS_CLASS_DECLARATION, astNode.getNodeType());
		assertNotNull("Didn't get a key", typeBinding.getKey());
		astNode = unit.findDeclaringNode(typeBinding.getKey());
		assertNotNull("Didn't get a declaring node", astNode);
		
		ITypeBinding typeBinding3 = classInstanceCreation.resolveTypeBinding();
		assertEquals("wrong binding", typeBinding, typeBinding3);
		
		List bodyDeclarations = anonymousClassDeclaration.bodyDeclarations();
		assertEquals("wrong size", 2, bodyDeclarations.size());
		BodyDeclaration bodyDeclaration = (BodyDeclaration) bodyDeclarations.get(0);
		assertTrue("not a type declaration", bodyDeclaration.getNodeType() == ASTNode.TYPE_DECLARATION);
		typeDeclaration = (TypeDeclaration) bodyDeclaration;
		
		bodyDeclaration = (BodyDeclaration) bodyDeclarations.get(1);
		MethodDeclaration methodDeclaration = (MethodDeclaration) bodyDeclaration;
		Block block = methodDeclaration.getBody();
		assertNotNull("No body", block);
		List statements = block.statements();
		assertEquals("wrong size", 2, statements.size());
		Statement statement = (Statement) statements.get(1);
		assertEquals("Not a variable declaration statement", statement.getNodeType(), ASTNode.VARIABLE_DECLARATION_STATEMENT);
		VariableDeclarationStatement variableDeclarationStatement = (VariableDeclarationStatement) statement;
		Type type = variableDeclarationStatement.getType();
		assertNotNull("No type", type);
		
		ITypeBinding typeBinding2 = type.resolveBinding();
		typeBinding = typeDeclaration.resolveBinding();
		assertTrue("not equals", typeBinding == typeBinding2);
		assertNotNull("No type binding", typeBinding);
		assertTrue("not a class", typeBinding.isClass());
		assertTrue("a toplevel type", !typeBinding.isTopLevel());
		assertTrue("an anonymous type", !typeBinding.isAnonymous());
		assertTrue("not a member type", typeBinding.isMember());
		assertTrue("not a nested type", typeBinding.isNested());		
		assertTrue("a local type", !typeBinding.isLocal());
		
		bodyDeclarations = typeDeclaration.bodyDeclarations();
		assertEquals("wrong size", 1, bodyDeclarations.size());
		bodyDeclaration = (BodyDeclaration) bodyDeclarations.get(0);
		assertTrue("not a type declaration", bodyDeclaration.getNodeType() == ASTNode.TYPE_DECLARATION);
		typeDeclaration = (TypeDeclaration) bodyDeclaration;
		typeBinding = typeDeclaration.resolveBinding();
		assertNotNull("No type binding", typeBinding);
		assertTrue("not a class", typeBinding.isClass());
		assertTrue("a toplevel type", !typeBinding.isTopLevel());
		assertTrue("an anonymous type", !typeBinding.isAnonymous());
		assertTrue("not a member type", typeBinding.isMember());
		assertTrue("not a nested type", typeBinding.isNested());		
		assertTrue("a local type", !typeBinding.isLocal());
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=24916
	 */
	public void test0451() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0451", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 2, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		Type type = methodDeclaration.getReturnType();
		checkSourceRange(type, "int", source);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=27204
	 */
	public void test0452() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "", "NO_WORKING.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, false);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit compilationUnit = (CompilationUnit) result;
		ASTNode node = getASTNode(compilationUnit, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		SimpleName name = methodDeclaration.getName();
		assertEquals("wrong line number", 3, compilationUnit.lineNumber(name.getStartPosition()));
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=27173
	 */
	public void test0453() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0453", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit compilationUnit = (CompilationUnit) result;
		ASTNode node = getASTNode(compilationUnit, 0, 0,0);
		assertNotNull("No node", node);
		assertTrue("not a return statement", node.getNodeType() == ASTNode.RETURN_STATEMENT); //$NON-NLS-1$
		ReturnStatement returnStatement = (ReturnStatement) node;
		Expression expression = returnStatement.getExpression();
		assertTrue("not a super method invocation", expression.getNodeType() == ASTNode.SUPER_METHOD_INVOCATION); //$NON-NLS-1$
		SuperMethodInvocation methodInvocation = (SuperMethodInvocation) expression;
		IMethodBinding methodBinding = methodInvocation.resolveMethodBinding();
		assertNotNull("No method binding", methodBinding);
		assertEquals("Wrong binding", "toString", methodBinding.getName());
	}	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=28296
	 */
	public void test0454() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0454", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit compilationUnit = (CompilationUnit) result;
		ASTNode node = getASTNode(compilationUnit, 0, 0,1);
		assertNotNull("No node", node);
		assertTrue("not a variable declaration statement", node.getNodeType() == ASTNode.VARIABLE_DECLARATION_STATEMENT); //$NON-NLS-1$
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node;
		List fragments = statement.fragments();
		assertEquals("wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertTrue("not a cast expression", expression.getNodeType() == ASTNode.CAST_EXPRESSION); //$NON-NLS-1$
		checkSourceRange(expression, "(int) (3.14f * a)", source);
		CastExpression castExpression = (CastExpression) expression;
		checkSourceRange(castExpression.getType(), "int", source);
		Expression expression2 = castExpression.getExpression();
		checkSourceRange(expression2, "(3.14f * a)", source);	
		assertTrue("not a parenthesized expression", expression2.getNodeType() == ASTNode.PARENTHESIZED_EXPRESSION); //$NON-NLS-1$
	}
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=28682
	 */
	public void test0455() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0455", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit compilationUnit = (CompilationUnit) result;
		ASTNode node = getASTNode(compilationUnit, 0, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a for statement", node.getNodeType() == ASTNode.FOR_STATEMENT); //$NON-NLS-1$
		ForStatement forStatement = (ForStatement) node; // first for loop
		String expectedSource = "for (int i = 0; i < 10; i++)  // for 1\n" +
			"	        for (int j = 0; j < 10; j++)  // for 2\n" +
			"	            if (true) { }";
		checkSourceRange(forStatement, expectedSource, source);
		Statement body = forStatement.getBody();
		expectedSource = "for (int j = 0; j < 10; j++)  // for 2\n" +
			"	            if (true) { }";
		checkSourceRange(body, expectedSource, source);		
		assertTrue("not a for statement", body.getNodeType() == ASTNode.FOR_STATEMENT); //$NON-NLS-1$
		ForStatement forStatement2 = (ForStatement) body;
		body = forStatement2.getBody();
		expectedSource = "if (true) { }";
		checkSourceRange(body, expectedSource, source);		
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=28682
	 */
	public void test0456() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0456", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit compilationUnit = (CompilationUnit) result;
		ASTNode node = getASTNode(compilationUnit, 0, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a for statement", node.getNodeType() == ASTNode.FOR_STATEMENT); //$NON-NLS-1$
		ForStatement forStatement = (ForStatement) node; // first for loop
		String expectedSource = "for (int x= 10; x < 20; x++)\n" +
			"			main();";
		checkSourceRange(forStatement, expectedSource, source);
		Statement body = forStatement.getBody();
		expectedSource = "main();";
		checkSourceRange(body, expectedSource, source);		
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=28682
	 */
	public void test0457() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0457", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit compilationUnit = (CompilationUnit) result;
		ASTNode node = getASTNode(compilationUnit, 0, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a for statement", node.getNodeType() == ASTNode.FOR_STATEMENT); //$NON-NLS-1$
		ForStatement forStatement = (ForStatement) node; // first for loop
		String expectedSource = "for (int i= 10; i < 10; i++)/*[*/\n"+
			"			for (int z= 10; z < 10; z++)\n" +
			"				foo();";
		checkSourceRange(forStatement, expectedSource, source);
		Statement body = forStatement.getBody();
		expectedSource = "for (int z= 10; z < 10; z++)\n" +
			"				foo();";
		checkSourceRange(body, expectedSource, source);		
		assertTrue("not a for statement", body.getNodeType() == ASTNode.FOR_STATEMENT); //$NON-NLS-1$
		ForStatement forStatement2 = (ForStatement) body;
		body = forStatement2.getBody();
		expectedSource = "foo();";
		checkSourceRange(body, expectedSource, source);		
	}	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=28682
	 */
	public void test0458() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0458", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit compilationUnit = (CompilationUnit) result;
		ASTNode node = getASTNode(compilationUnit, 0, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a for statement", node.getNodeType() == ASTNode.FOR_STATEMENT); //$NON-NLS-1$
		ForStatement forStatement = (ForStatement) node; // first for loop
		String expectedSource = "for (int i= 10; i < 10; i++)/*[*/\n"+
			"			for (int z= 10; z < 10; z++)\n" +
			"				;";
		checkSourceRange(forStatement, expectedSource, source);
		Statement body = forStatement.getBody();
		expectedSource = "for (int z= 10; z < 10; z++)\n" +
			"				;";
		checkSourceRange(body, expectedSource, source);		
		assertTrue("not a for statement", body.getNodeType() == ASTNode.FOR_STATEMENT); //$NON-NLS-1$
		ForStatement forStatement2 = (ForStatement) body;
		body = forStatement2.getBody();
		expectedSource = ";";
		checkSourceRange(body, expectedSource, source);		
		assertTrue("not an empty statement", body.getNodeType() == ASTNode.EMPTY_STATEMENT); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=28682
	 */
	public void test0459() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0459", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit compilationUnit = (CompilationUnit) result;
		ASTNode node = getASTNode(compilationUnit, 0, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a for statement", node.getNodeType() == ASTNode.FOR_STATEMENT); //$NON-NLS-1$
		ForStatement forStatement = (ForStatement) node; // first for loop
		String expectedSource = "for (int i= 10; i < 10; i++)/*[*/\n"+
			"			for (int z= 10; z < 10; z++)\n" +
			"				{    }";
		checkSourceRange(forStatement, expectedSource, source);
		Statement body = forStatement.getBody();
		expectedSource = "for (int z= 10; z < 10; z++)\n" +
			"				{    }";
		checkSourceRange(body, expectedSource, source);		
		assertTrue("not a for statement", body.getNodeType() == ASTNode.FOR_STATEMENT); //$NON-NLS-1$
		ForStatement forStatement2 = (ForStatement) body;
		body = forStatement2.getBody();
		expectedSource = "{    }";
		checkSourceRange(body, expectedSource, source);		
		assertTrue("not a block", body.getNodeType() == ASTNode.BLOCK); //$NON-NLS-1$
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=28869
	 */
	public void test0460() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0460", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit compilationUnit = (CompilationUnit) result;
		assertTrue("Has error", compilationUnit.getProblems().length == 0); //$NON-NLS-1$
		ASTNode node = getASTNode(compilationUnit, 0, 0, 0);
		assertNotNull("No node", node);
		assertTrue("Malformed", !isMalformed(node));
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=28824
	 */
	public void test0461() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0461", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, false);
		char[] source = sourceUnit.getSource().toCharArray();
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit compilationUnit = (CompilationUnit) result;
		assertTrue("Has error", compilationUnit.getProblems().length == 0); //$NON-NLS-1$
		ASTNode node = getASTNode(compilationUnit, 0, 0, 0);
		assertNotNull("No node", node);
		assertTrue("Malformed", !isMalformed(node));
		assertTrue("not an expression statement", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertTrue("not an assignment", expression.getNodeType() == ASTNode.ASSIGNMENT); //$NON-NLS-1$
		Assignment assignment = (Assignment) expression;
		checkSourceRange(assignment, "z= foo().y.toList()", source);
		Expression expression2 = assignment.getRightHandSide();
		checkSourceRange(expression2, "foo().y.toList()", source);
		assertTrue("not a method invocation", expression2.getNodeType() == ASTNode.METHOD_INVOCATION);
		MethodInvocation methodInvocation = (MethodInvocation) expression2;
		Expression expression3 = methodInvocation.getExpression();
		checkSourceRange(expression3, "foo().y", source);
		checkSourceRange(methodInvocation.getName(), "toList", source);
		assertTrue("not a field access", expression3.getNodeType() == ASTNode.FIELD_ACCESS);
		FieldAccess fieldAccess = (FieldAccess) expression3;
		checkSourceRange(fieldAccess.getName(), "y", source);
		Expression expression4 = fieldAccess.getExpression();
		checkSourceRange(expression4, "foo()", source);
		assertTrue("not a method invocation", expression4.getNodeType() == ASTNode.METHOD_INVOCATION);
		MethodInvocation methodInvocation2 = (MethodInvocation) expression4;
		checkSourceRange(methodInvocation2.getName(), "foo", source);
		assertNull("no null", methodInvocation2.getExpression());
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=32338
	 */
	public void test0462() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "", "Test462.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit compilationUnit = (CompilationUnit) result;
		assertTrue("Has error", compilationUnit.getProblems().length == 0); //$NON-NLS-1$
		ASTNode node = getASTNode(compilationUnit, 0);
		assertNotNull("No node", node);
		assertTrue("not a type declaration", node.getNodeType() == ASTNode.TYPE_DECLARATION);
		TypeDeclaration typeDeclaration = (TypeDeclaration) node;
		assertEquals("Wrong name", "Test462", typeDeclaration.getName().getIdentifier());
		ITypeBinding typeBinding = typeDeclaration.resolveBinding();
		assertNotNull("No binding", typeBinding);
		assertEquals("Wrong name", "Test462", typeBinding.getQualifiedName());
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=33450
	 */
	public void test0463() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0463", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, false);
		CompilationUnit compilationUnit = (CompilationUnit) result;
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode node = getASTNode(compilationUnit, 0, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a return statement", node.getNodeType() == ASTNode.RETURN_STATEMENT); //$NON-NLS-1$
		ReturnStatement returnStatement = (ReturnStatement) node;
		Expression expression = returnStatement.getExpression();
		assertNotNull("No expression", expression);
		assertTrue("not a string literal", expression.getNodeType() == ASTNode.STRING_LITERAL); //$NON-NLS-1$
		StringLiteral stringLiteral = (StringLiteral) expression;
		checkSourceRange(stringLiteral, "\"\\012\\015\\u0061\"", source);
		assertEquals("wrong value", "\012\015a", stringLiteral.getLiteralValue());
		assertEquals("wrong value", "\"\\012\\015\\u0061\"", stringLiteral.getEscapedValue());
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=33039
	 */
	public void test0464() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0464", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		CompilationUnit compilationUnit = (CompilationUnit) result;
		ASTNode node = getASTNode(compilationUnit, 0, 0, 0);
		assertEquals("No error", 1, compilationUnit.getProblems().length); //$NON-NLS-1$
		assertNotNull("No node", node);
		assertTrue("not a return statement", node.getNodeType() == ASTNode.RETURN_STATEMENT); //$NON-NLS-1$
		ReturnStatement returnStatement = (ReturnStatement) node;
		Expression expression = returnStatement.getExpression();
		assertNotNull("No expression", expression);
		assertTrue("not a null literal", expression.getNodeType() == ASTNode.NULL_LITERAL); //$NON-NLS-1$
		NullLiteral nullLiteral = (NullLiteral) expression;
		ITypeBinding typeBinding = nullLiteral.resolveTypeBinding();
		assertNotNull("No type binding", typeBinding);
		assertFalse("A primitive type", typeBinding.isPrimitive());
		assertTrue("Null type", typeBinding.isNullType());
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=33831
	 */
	public void test0465() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0465", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		CompilationUnit compilationUnit = (CompilationUnit) result;
		ASTNode node = getASTNode(compilationUnit, 0, 1, 0);
		assertEquals("No error", 0, compilationUnit.getProblems().length); //$NON-NLS-1$
		assertNotNull("No node", node);
		assertTrue("not a return statement", node.getNodeType() == ASTNode.RETURN_STATEMENT); //$NON-NLS-1$
		ReturnStatement returnStatement = (ReturnStatement) node;
		Expression expression = returnStatement.getExpression();
		assertNotNull("No expression", expression);
		assertTrue("not a field access", expression.getNodeType() == ASTNode.FIELD_ACCESS); //$NON-NLS-1$
		FieldAccess fieldAccess = (FieldAccess) expression;
		Name name = fieldAccess.getName();
		IBinding binding = name.resolveBinding();
		assertNotNull("No binding", binding);
		assertEquals("Wrong type", IBinding.VARIABLE, binding.getKind());
		IVariableBinding variableBinding = (IVariableBinding) binding;
		assertEquals("Wrong name", "i", variableBinding.getName());
		assertEquals("Wrong type", "int", variableBinding.getType().getName());
		IVariableBinding variableBinding2 = fieldAccess.resolveFieldBinding();
		assertTrue("different binding", variableBinding == variableBinding2);
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=33949
	 */
	public void test0466() throws JavaModelException {
		Hashtable options = JavaCore.getOptions();
		Hashtable newOptions = JavaCore.getOptions();
		try {
			newOptions.put(JavaCore.COMPILER_SOURCE, JavaCore.VERSION_1_4);
			JavaCore.setOptions(newOptions);
			ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0466", "Assert.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			ASTNode result = runConversion(sourceUnit, true);
			CompilationUnit compilationUnit = (CompilationUnit) result;
			char[] source = sourceUnit.getSource().toCharArray();
			ASTNode node = getASTNode(compilationUnit, 0, 0, 0);
			checkSourceRange(node, "assert ref != null : message;", source);
			assertTrue("not an assert statement", node.getNodeType() == ASTNode.ASSERT_STATEMENT); //$NON-NLS-1$
			AssertStatement statement = (AssertStatement) node;
			checkSourceRange(statement.getExpression(), "ref != null", source);
			checkSourceRange(statement.getMessage(), "message", source);
		} finally {
			JavaCore.setOptions(options);
		}
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=33949
	 */
	public void test0467() throws JavaModelException {
		Hashtable options = JavaCore.getOptions();
		Hashtable newOptions = JavaCore.getOptions();
		try {
			newOptions.put(JavaCore.COMPILER_SOURCE, JavaCore.VERSION_1_4);
			JavaCore.setOptions(newOptions);
			ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0467", "Assert.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			ASTNode result = runConversion(sourceUnit, true);
			CompilationUnit compilationUnit = (CompilationUnit) result;
			char[] source = sourceUnit.getSource().toCharArray();
			ASTNode node = getASTNode(compilationUnit, 0, 0, 0);
			checkSourceRange(node, "assert ref != null : message\\u003B", source);
			assertTrue("not an assert statement", node.getNodeType() == ASTNode.ASSERT_STATEMENT); //$NON-NLS-1$
			AssertStatement statement = (AssertStatement) node;
			checkSourceRange(statement.getExpression(), "ref != null", source);
			checkSourceRange(statement.getMessage(), "message", source);
			
			node = getASTNode(compilationUnit, 0, 0, 1);
			checkSourceRange(node, "assert ref != null\\u003B", source);
			assertTrue("not an assert statement", node.getNodeType() == ASTNode.ASSERT_STATEMENT); //$NON-NLS-1$
			statement = (AssertStatement) node;
			checkSourceRange(statement.getExpression(), "ref != null", source);
		} finally {
			JavaCore.setOptions(options);
		}
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=36772
	 */
	public void test0468() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0468", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		CompilationUnit compilationUnit = (CompilationUnit) result;
		ASTNode node = getASTNode(compilationUnit, 0, 1, 0);
		assertEquals("No error", 0, compilationUnit.getProblems().length); //$NON-NLS-1$
		assertNotNull("No node", node);
		assertTrue("not a return statement", node.getNodeType() == ASTNode.RETURN_STATEMENT); //$NON-NLS-1$
		ReturnStatement returnStatement = (ReturnStatement) node;
		Expression expression = returnStatement.getExpression();
		assertNotNull("No expression", expression);
		assertTrue("not a field access", expression.getNodeType() == ASTNode.FIELD_ACCESS); //$NON-NLS-1$
		FieldAccess fieldAccess = (FieldAccess) expression;
		Name name = fieldAccess.getName();
		IBinding binding = name.resolveBinding();
		assertNotNull("No binding", binding);
		assertEquals("Wrong type", IBinding.VARIABLE, binding.getKind());
		IVariableBinding variableBinding = (IVariableBinding) binding;
		assertEquals("Wrong name", "i", variableBinding.getName());
		assertEquals("Wrong type", "int", variableBinding.getType().getName());
		IVariableBinding variableBinding2 = fieldAccess.resolveFieldBinding();
		assertTrue("different binding", variableBinding == variableBinding2);
		
		node = getASTNode(compilationUnit, 0, 0);
		assertNotNull("No node", node);
		assertEquals("Wrong type", ASTNode.FIELD_DECLARATION, node.getNodeType());
		FieldDeclaration fieldDeclaration = (FieldDeclaration) node;
		List fragments = fieldDeclaration.fragments();
		assertEquals("wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		
		ASTNode foundNode = compilationUnit.findDeclaringNode(variableBinding);
		assertNotNull("No found node", foundNode);
		assertEquals("wrong node", fragment, foundNode);
	}		

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=36895
	 */
	public void test0469() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "codeManipulation", "bug.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		CompilationUnit compilationUnit = (CompilationUnit) result;
		ASTNode node = getASTNode(compilationUnit, 0, 2, 0);
		assertEquals("No error", 0, compilationUnit.getProblems().length); //$NON-NLS-1$
		assertNotNull("No node", node);
		assertTrue("not a variable declaration statement", node.getNodeType() == ASTNode.VARIABLE_DECLARATION_STATEMENT); //$NON-NLS-1$
		ASTNode parent = node.getParent();
		assertNotNull(parent);
		assertTrue("not a block", parent.getNodeType() == ASTNode.BLOCK); //$NON-NLS-1$
	}		

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=37381
	 */
	public void test0470() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0470", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		CompilationUnit compilationUnit = (CompilationUnit) result;
		ASTNode node = getASTNode(compilationUnit, 0, 0, 0);
		assertEquals("No error", 0, compilationUnit.getProblems().length); //$NON-NLS-1$
		assertNotNull("No node", node);
		assertTrue("not a for statement", node.getNodeType() == ASTNode.FOR_STATEMENT); //$NON-NLS-1$
		ForStatement forStatement = (ForStatement) node;
		List initializers = forStatement.initializers();
		assertEquals("wrong size", 1, initializers.size());
		Expression initializer = (Expression) initializers.get(0);
		assertTrue("not a variable declaration expression", initializer.getNodeType() == ASTNode.VARIABLE_DECLARATION_EXPRESSION); //$NON-NLS-1$
		VariableDeclarationExpression variableDeclarationExpression = (VariableDeclarationExpression) initializer;
		List fragments = variableDeclarationExpression.fragments();
		assertEquals("wrong size", 2, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		checkSourceRange(fragment, "i= 0", source);
		fragment = (VariableDeclarationFragment) fragments.get(1);
		checkSourceRange(fragment, "j= goo(3)", source);
		checkSourceRange(variableDeclarationExpression, "int i= 0, j= goo(3)", source);
	}		

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=38447
	 */
	public void test0471() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0471", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		CompilationUnit compilationUnit = (CompilationUnit) result;
		assertEquals("No error", 1, compilationUnit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(compilationUnit, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertTrue("Is a constructor", !methodDeclaration.isConstructor());
		checkSourceRange(methodDeclaration, "private void foo(){", source);
		node = getASTNode(compilationUnit, 0, 1);
		assertNotNull("No node", node);
		assertTrue("not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		methodDeclaration = (MethodDeclaration) node;
		assertTrue("Is a constructor", !methodDeclaration.isConstructor());
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=38447
	 */
	public void test0472() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "junit.textui", "ResultPrinter.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		CompilationUnit compilationUnit = (CompilationUnit) result;
		assertEquals("No error", 2, compilationUnit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(compilationUnit, 0, 2);
		assertNotNull("No node", node);
		assertTrue("not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertTrue("Not a constructor", methodDeclaration.isConstructor());
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=38732
	 */
	public void test0473() throws JavaModelException {
		Hashtable options = JavaCore.getOptions();
		Hashtable newOptions = JavaCore.getOptions();
		try {
			newOptions.put(JavaCore.COMPILER_SOURCE, JavaCore.VERSION_1_4);
			newOptions.put(JavaCore.COMPILER_PB_ASSERT_IDENTIFIER, JavaCore.ERROR);
			JavaCore.setOptions(newOptions);
				
			ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0473", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			char[] source = sourceUnit.getSource().toCharArray();
			ASTNode result = runConversion(sourceUnit, true);
			CompilationUnit compilationUnit = (CompilationUnit) result;
			assertEquals("No error", 2, compilationUnit.getProblems().length); //$NON-NLS-1$
			ASTNode node = getASTNode(compilationUnit, 0, 0, 0);
			assertNotNull("No node", node);
			assertTrue("not an assert statement", node.getNodeType() == ASTNode.ASSERT_STATEMENT); //$NON-NLS-1$
			AssertStatement assertStatement = (AssertStatement) node;
			checkSourceRange(assertStatement, "assert(true);", source);
			Expression expression = assertStatement.getExpression();
			checkSourceRange(expression, "(true)", source);
		} finally {
			JavaCore.setOptions(options);
		}
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=39259
	 */
	public void test0474() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0474", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		CompilationUnit compilationUnit = (CompilationUnit) result;
		assertEquals("No error", 0, compilationUnit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(compilationUnit, 0, 1, 0);
		assertNotNull("No node", node);
		assertEquals("Not a while statement", node.getNodeType(), ASTNode.WHILE_STATEMENT);
		WhileStatement whileStatement = (WhileStatement) node;
		Statement statement = whileStatement.getBody();
		assertEquals("Not a while statement", statement.getNodeType(), ASTNode.WHILE_STATEMENT);
		WhileStatement whileStatement2 = (WhileStatement) statement;
		String expectedSource = 
			"while(b())\n" +
			"				foo();";
		checkSourceRange(whileStatement2, expectedSource, source);
		Statement statement2 = whileStatement2.getBody();
		checkSourceRange(statement2, "foo();", source);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=39259
	 */
	public void test0475() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0475", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		CompilationUnit compilationUnit = (CompilationUnit) result;
		assertEquals("No error", 0, compilationUnit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(compilationUnit, 0, 1, 0);
		assertNotNull("No node", node);
		assertEquals("Not an if statement", node.getNodeType(), ASTNode.IF_STATEMENT);
		IfStatement statement = (IfStatement) node;
		Statement statement2 = statement.getThenStatement();
		assertEquals("Not an if statement", statement2.getNodeType(), ASTNode.IF_STATEMENT);
		IfStatement statement3 = (IfStatement) statement2;
		String expectedSource = 
			"if(b())\n" +
			"				foo();";
		checkSourceRange(statement3, expectedSource, source);
		Statement statement4 = statement3.getThenStatement();
		checkSourceRange(statement4, "foo();", source);
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=39259
	 */
	public void test0476() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0476", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		CompilationUnit compilationUnit = (CompilationUnit) result;
		assertEquals("No error", 0, compilationUnit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(compilationUnit, 0, 1, 0);
		assertNotNull("No node", node);
		assertEquals("Not a for statement", node.getNodeType(), ASTNode.FOR_STATEMENT);
		ForStatement statement = (ForStatement) node;
		Statement statement2 = statement.getBody();
		assertEquals("Not a for statement", statement2.getNodeType(), ASTNode.FOR_STATEMENT);
		ForStatement statement3 = (ForStatement) statement2;
		String expectedSource = 
			"for(;b();)\n" +
			"				foo();";
		checkSourceRange(statement3, expectedSource, source);
		Statement statement4 = statement3.getBody();
		checkSourceRange(statement4, "foo();", source);
	}	

	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=39327
	 * TODO reactivate when a binding is returned
	 */
	public void _test0477() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0477", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		CompilationUnit compilationUnit = (CompilationUnit) result;
		assertEquals("No error", 1, compilationUnit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(compilationUnit, 0, 1, 0);
		assertNotNull("No node", node);

		checkSourceRange(node, "this(undef());", source);
		assertEquals("Not a constructor invocation", node.getNodeType(), ASTNode.CONSTRUCTOR_INVOCATION);		
		ConstructorInvocation constructorInvocation = (ConstructorInvocation) node;
		List arguments = constructorInvocation.arguments();
		assertEquals("Wrong size", 1, arguments.size());
		IMethodBinding binding = constructorInvocation.resolveConstructorBinding();
		assertNotNull("No binding", binding);
	}

	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=40474
	 */
	public void test0478() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0478", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		IType[] types = sourceUnit.getTypes();
		assertNotNull(types);
		assertEquals("wrong size", 2, types.length);
		IType type = types[1];
		IMethod[] methods = type.getMethods();
		assertNotNull(methods);
		assertEquals("wrong size", 1, methods.length);
		IMethod method = methods[0];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(sourceUnit, sourceRange.getOffset() + sourceRange.getLength() / 2, true);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 1, 0);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertEquals("wrong name", "test", methodDeclaration.getName().getIdentifier());
		IMethodBinding methodBinding = methodDeclaration.resolveBinding();
		assertNotNull(methodBinding);
		List statements = ((MethodDeclaration) node).getBody().statements();
		assertEquals("wrong size", 2, statements.size());
		ASTNode node2 = (ASTNode) statements.get(1);
		assertNotNull(node2);
		assertTrue("Not an expression statement", node2.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node2;
		Expression expression = expressionStatement.getExpression();
		assertTrue("Not a method invocation", expression.getNodeType() == ASTNode.METHOD_INVOCATION); //$NON-NLS-1$
		MethodInvocation methodInvocation = (MethodInvocation) expression;
		Expression expression2 = methodInvocation.getExpression();
		assertTrue("Not a simple name", expression2.getNodeType() == ASTNode.SIMPLE_NAME); //$NON-NLS-1$
		SimpleName simpleName = (SimpleName) expression2;
		IBinding binding  = simpleName.resolveBinding();
		assertNotNull("No binding", binding); //$NON-NLS-1$
		assertTrue("wrong type", binding.getKind() == IBinding.VARIABLE); //$NON-NLS-1$
		IVariableBinding variableBinding = (IVariableBinding) binding;
		assertEquals("Wrong name", "a", variableBinding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
		SimpleName simpleName2 = methodInvocation.getName();
		assertEquals("Wrong name", "clone", simpleName2.getIdentifier()); //$NON-NLS-1$ //$NON-NLS-2$
		IBinding binding2 = simpleName2.resolveBinding();
		assertNotNull("no binding2", binding2); //$NON-NLS-1$
		assertTrue("Wrong type", binding2.getKind() == IBinding.METHOD); //$NON-NLS-1$
		IMethodBinding methodBinding2 = (IMethodBinding) binding2;
		assertEquals("Wrong name", "clone", methodBinding2.getName()); //$NON-NLS-1$ //$NON-NLS-2$
	}	

	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=40474
	 */
	public void test0479() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0479", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		IType[] types = sourceUnit.getTypes();
		assertNotNull(types);
		assertEquals("wrong size", 2, types.length);
		IType type = types[1];
		IMethod[] methods = type.getMethods();
		assertNotNull(methods);
		assertEquals("wrong size", 1, methods.length);
		IMethod method = methods[0];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(sourceUnit, sourceRange.getOffset() + sourceRange.getLength() / 2, false);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 1, 0);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertEquals("wrong name", "test", methodDeclaration.getName().getIdentifier());
		List statements = ((MethodDeclaration) node).getBody().statements();
		assertEquals("wrong size", 2, statements.size());
		ASTNode node2 = (ASTNode) statements.get(1);
		assertNotNull(node2);
		assertTrue("Not an expression statement", node2.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node2;
		Expression expression = expressionStatement.getExpression();
		assertTrue("Not a method invocation", expression.getNodeType() == ASTNode.METHOD_INVOCATION); //$NON-NLS-1$
		MethodInvocation methodInvocation = (MethodInvocation) expression;
		Expression expression2 = methodInvocation.getExpression();
		assertTrue("Not a simple name", expression2.getNodeType() == ASTNode.SIMPLE_NAME); //$NON-NLS-1$
		SimpleName simpleName = (SimpleName) expression2;
		IBinding binding  = simpleName.resolveBinding();
		assertNull("No binding", binding); //$NON-NLS-1$
		SimpleName simpleName2 = methodInvocation.getName();
		assertEquals("Wrong name", "clone", simpleName2.getIdentifier()); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=40474
	 */
	public void test0480() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0480", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		IType[] types = sourceUnit.getTypes();
		assertNotNull(types);
		assertEquals("wrong size", 1, types.length);
		IType type = types[0];
		IMethod[] methods = type.getMethods();
		assertNotNull(methods);
		assertEquals("wrong size", 1, methods.length);
		IMethod method = methods[0];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(sourceUnit, sourceRange.getOffset() + sourceRange.getLength() / 2, false);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertEquals("wrong name", "test", methodDeclaration.getName().getIdentifier());
		List statements = ((MethodDeclaration) node).getBody().statements();
		assertEquals("wrong size", 1, statements.size());
		ASTNode node2 = (ASTNode) statements.get(0);
		assertNotNull(node2);
		assertTrue("Not an variable declaration statement", node2.getNodeType() == ASTNode.VARIABLE_DECLARATION_STATEMENT); //$NON-NLS-1$
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=40474
	 */
	public void test0481() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0481", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		IType[] types = sourceUnit.getTypes();
		assertNotNull(types);
		assertEquals("wrong size", 1, types.length);
		IType type = types[0];
		IMethod[] methods = type.getMethods();
		assertNotNull(methods);
		assertEquals("wrong size", 1, methods.length);
		IMethod method = methods[0];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(sourceUnit, sourceRange.getOffset() + sourceRange.getLength() / 2, true);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertEquals("wrong name", "test", methodDeclaration.getName().getIdentifier());
		List statements = ((MethodDeclaration) node).getBody().statements();
		assertEquals("wrong size", 1, statements.size());
		ASTNode node2 = (ASTNode) statements.get(0);
		assertNotNull(node2);
		assertTrue("Not an variable declaration statement", node2.getNodeType() == ASTNode.VARIABLE_DECLARATION_STATEMENT); //$NON-NLS-1$
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node2;
		List fragments = statement.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertTrue("Not a class instance creation", expression.getNodeType() == ASTNode.CLASS_INSTANCE_CREATION); //$NON-NLS-1$
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expression;
		ITypeBinding typeBinding = classInstanceCreation.resolveTypeBinding();
		assertNotNull(typeBinding);
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=40474
	 */
	public void test0482() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0482", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		IType[] types = sourceUnit.getTypes();
		assertNotNull(types);
		assertEquals("wrong size", 1, types.length);
		IType type = types[0];
		IType[] memberTypes = type.getTypes();
		assertNotNull(memberTypes);
		assertEquals("wrong size", 1, memberTypes.length);
		IType memberType = memberTypes[0];
		IMethod[] methods = memberType.getMethods();
		assertEquals("wrong size", 1, methods.length);
		IMethod method = methods[0];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(sourceUnit, sourceRange.getOffset() + sourceRange.getLength() / 2, true);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0, 0);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertEquals("wrong name", "test", methodDeclaration.getName().getIdentifier());
		List statements = ((MethodDeclaration) node).getBody().statements();
		assertEquals("wrong size", 1, statements.size());
		ASTNode node2 = (ASTNode) statements.get(0);
		assertNotNull(node2);
		assertTrue("Not an variable declaration statement", node2.getNodeType() == ASTNode.VARIABLE_DECLARATION_STATEMENT); //$NON-NLS-1$
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node2;
		List fragments = statement.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertTrue("Not a class instance creation", expression.getNodeType() == ASTNode.CLASS_INSTANCE_CREATION); //$NON-NLS-1$
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expression;
		ITypeBinding typeBinding = classInstanceCreation.resolveTypeBinding();
		assertNotNull(typeBinding);
		assertTrue(typeBinding.isAnonymous());
		assertEquals("Wrong name", "", typeBinding.getName());
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=40474
	 */
	public void test0483() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0483", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		IType[] types = sourceUnit.getTypes();
		assertNotNull(types);
		assertEquals("wrong size", 1, types.length);
		IType type = types[0];
		IMethod[] methods = type.getMethods();
		assertEquals("wrong size", 1, methods.length);
		IMethod method = methods[0];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(sourceUnit, sourceRange.getOffset() + sourceRange.getLength() / 2, true);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertEquals("wrong name", "A", methodDeclaration.getName().getIdentifier());
		assertTrue("Not a constructor", methodDeclaration.isConstructor());
		IBinding binding = methodDeclaration.getName().resolveBinding();
		assertNotNull(binding);
		assertEquals("Wrong type", IBinding.METHOD, binding.getKind());
		List statements = ((MethodDeclaration) node).getBody().statements();
		assertEquals("wrong size", 1, statements.size());
		ASTNode node2 = (ASTNode) statements.get(0);
		assertNotNull(node2);
		assertTrue("Not an variable declaration statement", node2.getNodeType() == ASTNode.VARIABLE_DECLARATION_STATEMENT); //$NON-NLS-1$
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node2;
		List fragments = statement.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertTrue("Not a class instance creation", expression.getNodeType() == ASTNode.CLASS_INSTANCE_CREATION); //$NON-NLS-1$
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expression;
		ITypeBinding typeBinding = classInstanceCreation.resolveTypeBinding();
		assertNotNull(typeBinding);
		assertTrue(typeBinding.isAnonymous());
		assertEquals("Wrong name", "", typeBinding.getName());
	}

	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=40474
	 */
	public void test0484() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0482", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		IType[] types = sourceUnit.getTypes();
		assertNotNull(types);
		assertEquals("wrong size", 1, types.length);
		IType type = types[0];
		IType[] memberTypes = type.getTypes();
		assertNotNull(memberTypes);
		assertEquals("wrong size", 1, memberTypes.length);
		IType memberType = memberTypes[0];
		ISourceRange sourceRange = memberType.getSourceRange(); 
		ASTNode result = runConversion(sourceUnit, sourceRange.getOffset() + sourceRange.getLength() / 2, true);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a type declaration", node.getNodeType() == ASTNode.TYPE_DECLARATION); //$NON-NLS-1$
		TypeDeclaration typeDeclaration = (TypeDeclaration) node;
		assertEquals("wrong name", "B", typeDeclaration.getName().getIdentifier());
		List bodyDeclarations = typeDeclaration.bodyDeclarations();
		assertEquals("Wrong size", 1, bodyDeclarations.size());
		BodyDeclaration bodyDeclaration = (BodyDeclaration) bodyDeclarations.get(0);
		assertTrue("Not a method declaration", bodyDeclaration.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) bodyDeclaration;
		Block block = methodDeclaration.getBody();
		List statements = block.statements();
		assertEquals("Wrong size", 1, statements.size());
	}

	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=40474
	 */
	public void test0485() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0482", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		IType[] types = sourceUnit.getTypes();
		assertNotNull(types);
		assertEquals("wrong size", 1, types.length);
		IType type = types[0];
		IType[] memberTypes = type.getTypes();
		assertNotNull(memberTypes);
		assertEquals("wrong size", 1, memberTypes.length);
		IType memberType = memberTypes[0];
		ISourceRange sourceRange = memberType.getSourceRange(); 
		ASTNode result = runConversion(sourceUnit, sourceRange.getOffset() + sourceRange.getLength() / 2, false);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a type declaration", node.getNodeType() == ASTNode.TYPE_DECLARATION); //$NON-NLS-1$
		TypeDeclaration typeDeclaration = (TypeDeclaration) node;
		assertEquals("wrong name", "B", typeDeclaration.getName().getIdentifier());
		List bodyDeclarations = typeDeclaration.bodyDeclarations();
		assertEquals("Wrong size", 1, bodyDeclarations.size());
		BodyDeclaration bodyDeclaration = (BodyDeclaration) bodyDeclarations.get(0);
		assertTrue("Not a method declaration", bodyDeclaration.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) bodyDeclaration;
		Block block = methodDeclaration.getBody();
		List statements = block.statements();
		assertEquals("Wrong size", 1, statements.size());
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=40474
	 */
	public void test0486() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0486", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		IType[] types = sourceUnit.getTypes();
		assertNotNull(types);
		assertEquals("wrong size", 1, types.length);
		IType type = types[0];
		IMethod[] methods = type.getMethods();
		assertEquals("wrong size", 2, methods.length);
		IMethod method = methods[1];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(sourceUnit, sourceRange.getOffset() + sourceRange.getLength() / 2, false);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 2);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		Block block = methodDeclaration.getBody();
		List statements = block.statements();
		assertEquals("Wrong size", 2, statements.size());

		node = getASTNode((CompilationUnit) result, 0, 1);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		methodDeclaration = (MethodDeclaration) node;
		block = methodDeclaration.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
	}

	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=40474
	 */
	public void test0487() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0487", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		IType[] types = sourceUnit.getTypes();
		assertNotNull(types);
		assertEquals("wrong size", 1, types.length);
		IType type = types[0];
		IMethod[] methods = type.getMethods();
		assertEquals("wrong size", 3, methods.length);
		IMethod method = methods[1];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(sourceUnit, sourceRange.getOffset() + sourceRange.getLength() / 2, false);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$

		ASTNode node = getASTNode((CompilationUnit) result, 0, 5);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		Block block = methodDeclaration.getBody();
		List statements = block.statements();
		assertEquals("Wrong size", 2, statements.size());

		node = getASTNode((CompilationUnit) result, 0, 4);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		methodDeclaration = (MethodDeclaration) node;
		block = methodDeclaration.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
		
		node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION); //$NON-NLS-1$
		FieldDeclaration fieldDeclaration = (FieldDeclaration) node;
		List fragments = fieldDeclaration.fragments();
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertEquals("Wrong name", "field", fragment.getName().getIdentifier());
		assertNotNull("No initializer", expression);

		node = getASTNode((CompilationUnit) result, 0, 1);
		assertTrue("Not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION); //$NON-NLS-1$
		fieldDeclaration = (FieldDeclaration) node;
		fragments = fieldDeclaration.fragments();
		fragment = (VariableDeclarationFragment) fragments.get(0);
		expression = fragment.getInitializer();
		assertEquals("Wrong name", "i", fragment.getName().getIdentifier());
		assertNotNull("No initializer", expression);

		node = getASTNode((CompilationUnit) result, 0, 2);
		assertTrue("Not an initializer", node.getNodeType() == ASTNode.INITIALIZER); //$NON-NLS-1$
		Initializer initializer = (Initializer) node;
		assertEquals("Not static", Modifier.NONE, initializer.getModifiers());
		block = initializer.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
		
		node = getASTNode((CompilationUnit) result, 0, 3);
		assertTrue("Not an initializer", node.getNodeType() == ASTNode.INITIALIZER); //$NON-NLS-1$
		initializer = (Initializer) node;
		assertEquals("Not static", Modifier.STATIC, initializer.getModifiers());
		block = initializer.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
		
		node = getASTNode((CompilationUnit) result, 0, 6);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		methodDeclaration = (MethodDeclaration) node;
		block = methodDeclaration.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
	}	

	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=40474
	 */
	public void test0488() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0488", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		IType[] types = sourceUnit.getTypes();
		assertNotNull(types);
		assertEquals("wrong size", 1, types.length);
		IType type = types[0];
		IInitializer[] initializers = type.getInitializers();
		assertEquals("wrong size", 2, initializers.length);
		IInitializer init = initializers[1];
		ISourceRange sourceRange = init.getSourceRange(); 
		ASTNode result = runConversion(sourceUnit, sourceRange.getOffset() + sourceRange.getLength() / 2, false);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$

		ASTNode node = getASTNode((CompilationUnit) result, 0, 5);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		Block block = methodDeclaration.getBody();
		List statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());

		node = getASTNode((CompilationUnit) result, 0, 4);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		methodDeclaration = (MethodDeclaration) node;
		block = methodDeclaration.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
		
		node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION); //$NON-NLS-1$
		FieldDeclaration fieldDeclaration = (FieldDeclaration) node;
		List fragments = fieldDeclaration.fragments();
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertEquals("Wrong name", "field", fragment.getName().getIdentifier());
		assertNotNull("No initializer", expression);

		node = getASTNode((CompilationUnit) result, 0, 1);
		assertTrue("Not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION); //$NON-NLS-1$
		fieldDeclaration = (FieldDeclaration) node;
		fragments = fieldDeclaration.fragments();
		fragment = (VariableDeclarationFragment) fragments.get(0);
		expression = fragment.getInitializer();
		assertEquals("Wrong name", "i", fragment.getName().getIdentifier());
		assertNotNull("No initializer", expression);

		node = getASTNode((CompilationUnit) result, 0, 2);
		assertTrue("Not an initializer", node.getNodeType() == ASTNode.INITIALIZER); //$NON-NLS-1$
		Initializer initializer = (Initializer) node;
		assertEquals("Not static", Modifier.NONE, initializer.getModifiers());
		block = initializer.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
		
		node = getASTNode((CompilationUnit) result, 0, 3);
		assertTrue("Not an initializer", node.getNodeType() == ASTNode.INITIALIZER); //$NON-NLS-1$
		initializer = (Initializer) node;
		assertEquals("Not static", Modifier.STATIC, initializer.getModifiers());
		block = initializer.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 1, statements.size());
		
		node = getASTNode((CompilationUnit) result, 0, 6);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		methodDeclaration = (MethodDeclaration) node;
		block = methodDeclaration.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
	}	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=40804
	 */
	public void test0489() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0489", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a type declaration", node.getNodeType() == ASTNode.TYPE_DECLARATION); //$NON-NLS-1$
		TypeDeclaration typeDeclaration = (TypeDeclaration) node;
		assertNull("Got a type binding", typeDeclaration.resolveBinding()); 
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=40804
	 */
	public void test0490() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0490", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=42647
	 */
	public void test0491() throws JavaModelException {
		Hashtable options = JavaCore.getOptions();
		Hashtable newOptions = JavaCore.getOptions();
		try {
			newOptions.put(JavaCore.COMPILER_SOURCE, JavaCore.VERSION_1_4);
			JavaCore.setOptions(newOptions);
			ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0491", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			char[] source = sourceUnit.getSource().toCharArray();
			ASTNode result = runConversion(sourceUnit, true);
			assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
			CompilationUnit unit = (CompilationUnit) result;
			assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
			ASTNode node = getASTNode(unit, 0, 0, 0);
			assertTrue("not an assert statement", node.getNodeType() == ASTNode.ASSERT_STATEMENT); //$NON-NLS-1$
			AssertStatement assertStatement = (AssertStatement) node;
			Expression expression = assertStatement.getExpression();
			assertTrue("not a parenthesized expression", expression.getNodeType() == ASTNode.PARENTHESIZED_EXPRESSION); //$NON-NLS-1$
			checkSourceRange(expression, "(loginName != null)", source);
		} finally {
			JavaCore.setOptions(options);
		}
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=42647
	 */
	public void test0492() throws JavaModelException {
		Hashtable options = JavaCore.getOptions();
		Hashtable newOptions = JavaCore.getOptions();
		try {
			newOptions.put(JavaCore.COMPILER_SOURCE, JavaCore.VERSION_1_4);
			JavaCore.setOptions(newOptions);
			ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0492", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			char[] source = sourceUnit.getSource().toCharArray();
			ASTNode result = runConversion(sourceUnit, true);
			assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
			CompilationUnit unit = (CompilationUnit) result;
			assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
			ASTNode node = getASTNode(unit, 0, 0, 0);
			assertTrue("not an assert statement", node.getNodeType() == ASTNode.ASSERT_STATEMENT); //$NON-NLS-1$
			AssertStatement assertStatement = (AssertStatement) node;
			Expression expression = assertStatement.getExpression();
			checkSourceRange(expression, "loginName != null", source);
		} finally {
			JavaCore.setOptions(options);
		}
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=42839
	 */
	public void test0493() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0493", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0);
		assertTrue("not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION); //$NON-NLS-1$
		FieldDeclaration fieldDeclaration = (FieldDeclaration) node;
		Type type = fieldDeclaration.getType();
		checkSourceRange(type, "Class[][]", source);
		assertTrue("not an array type", type.isArrayType()); //$NON-NLS-1$
		ArrayType arrayType = (ArrayType) type;
		Type componentType = arrayType.getComponentType();
		assertTrue("not an array type", componentType.isArrayType()); //$NON-NLS-1$
		checkSourceRange(componentType, "Class[]", source);
		arrayType = (ArrayType) componentType;
		componentType = arrayType.getComponentType();
		assertTrue("is an array type", !componentType.isArrayType()); //$NON-NLS-1$
		checkSourceRange(componentType, "Class", source);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=42839
	 */
	public void test0494() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0494", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0);
		assertTrue("not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION); //$NON-NLS-1$
		FieldDeclaration fieldDeclaration = (FieldDeclaration) node;
		Type type = fieldDeclaration.getType();
		checkSourceRange(type, "Class[][][]", source);
		assertTrue("not an array type", type.isArrayType()); //$NON-NLS-1$
		ArrayType arrayType = (ArrayType) type;
		Type componentType = arrayType.getComponentType();
		assertTrue("not an array type", componentType.isArrayType()); //$NON-NLS-1$
		checkSourceRange(componentType, "Class[][]", source);
		arrayType = (ArrayType) componentType;
		componentType = arrayType.getComponentType();
		assertTrue("not an array type", componentType.isArrayType()); //$NON-NLS-1$
		checkSourceRange(componentType, "Class[]", source);
		arrayType = (ArrayType) componentType;
		componentType = arrayType.getComponentType();
		assertTrue("is an array type", !componentType.isArrayType()); //$NON-NLS-1$
		checkSourceRange(componentType, "Class", source);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=42839
	 */
	public void test0495() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0495", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0);
		assertTrue("not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION); //$NON-NLS-1$
		FieldDeclaration fieldDeclaration = (FieldDeclaration) node;
		Type type = fieldDeclaration.getType();
		checkSourceRange(type, "Class[][]", source);
		assertTrue("not an array type", type.isArrayType()); //$NON-NLS-1$
		ArrayType arrayType = (ArrayType) type;
		Type componentType = arrayType.getComponentType();
		assertTrue("not an array type", componentType.isArrayType()); //$NON-NLS-1$
		checkSourceRange(componentType, "Class[]", source);
		arrayType = (ArrayType) componentType;
		componentType = arrayType.getComponentType();
		assertTrue("is an array type", !componentType.isArrayType()); //$NON-NLS-1$
		checkSourceRange(componentType, "Class", source);
		List fragments = fieldDeclaration.fragments();
		assertEquals("wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		assertEquals("wrong extra dimension", 1, fragment.getExtraDimensions());
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=42839
	 */
	public void test0496() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0496", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0);
		assertTrue("not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION); //$NON-NLS-1$
		FieldDeclaration fieldDeclaration = (FieldDeclaration) node;
		Type type = fieldDeclaration.getType();
		checkSourceRange(type, "Class[][][][]", source);
		assertTrue("not an array type", type.isArrayType()); //$NON-NLS-1$
		ArrayType arrayType = (ArrayType) type;
		Type componentType = arrayType.getComponentType();
		assertTrue("not an array type", componentType.isArrayType()); //$NON-NLS-1$
		checkSourceRange(componentType, "Class[][][]", source);
		arrayType = (ArrayType) componentType;
		componentType = arrayType.getComponentType();
		assertTrue("not an array type", componentType.isArrayType()); //$NON-NLS-1$
		checkSourceRange(componentType, "Class[][]", source);
		arrayType = (ArrayType) componentType;
		componentType = arrayType.getComponentType();
		assertTrue("not an array type", componentType.isArrayType()); //$NON-NLS-1$
		checkSourceRange(componentType, "Class[]", source);
		arrayType = (ArrayType) componentType;
		componentType = arrayType.getComponentType();
		assertTrue("is an array type", !componentType.isArrayType()); //$NON-NLS-1$
		checkSourceRange(componentType, "Class", source);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=42839
	 */
	public void test0497() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0497", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$<
		ASTNode node = getASTNode(unit, 0, 0);
		assertTrue("not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION); //$NON-NLS-1$
		FieldDeclaration fieldDeclaration = (FieldDeclaration) node;
		Type type = fieldDeclaration.getType();
		checkSourceRange(type, "Class[]", source);
		assertTrue("not an array type", type.isArrayType()); //$NON-NLS-1$
		ArrayType arrayType = (ArrayType) type;
		Type componentType = arrayType.getComponentType();
		assertTrue("is an array type", !componentType.isArrayType()); //$NON-NLS-1$
		checkSourceRange(componentType, "Class", source);
	}

	/**
	 */
	public void test0498() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0498", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=45199
	 */
	public void test0499() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0499", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0, 0, 1);
		assertNotNull(node);
		assertTrue("Not an expression statement", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		Expression expression = ((ExpressionStatement) node).getExpression();
		assertTrue("Not an assignment", expression.getNodeType() == ASTNode.ASSIGNMENT); //$NON-NLS-1$
		Assignment assignment = (Assignment) expression;
		Expression expression2 = assignment.getRightHandSide();
		assertTrue("Not an infix expression", expression2.getNodeType() == ASTNode.INFIX_EXPRESSION); //$NON-NLS-1$
		InfixExpression infixExpression = (InfixExpression) expression2;
		Expression expression3 = infixExpression.getLeftOperand();
		assertTrue("Not a simple name", expression3.getNodeType() == ASTNode.SIMPLE_NAME); //$NON-NLS-1$
		ITypeBinding binding = expression3.resolveTypeBinding();
		assertNotNull("No binding", binding);
		Expression expression4 = assignment.getLeftHandSide();
		assertTrue("Not a simple name", expression4.getNodeType() == ASTNode.SIMPLE_NAME); //$NON-NLS-1$
		ITypeBinding binding2 = expression4.resolveTypeBinding();
		assertNotNull("No binding", binding2);
		assertTrue("Should be the same", binding == binding2);
	}
	
	/**
	 * Test for bug 45436 fix.
	 * When this bug happened, the first assertion was false (2 problems found).
	 * @see <a href="http://bugs.eclipse.org/bugs/show_bug.cgi?id=45436">bug 45436</a>
	 * @throws JavaModelException
	 */
	public void test0500() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0500", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		IJavaProject project = sourceUnit.getJavaProject();
		Map originalOptions = project.getOptions(true);
		try {
			project.setOption(JavaCore.COMPILER_PB_INVALID_JAVADOC, JavaCore.ERROR);
			project.setOption(JavaCore.COMPILER_PB_MISSING_JAVADOC_TAGS, JavaCore.ERROR);
			project.setOption(JavaCore.COMPILER_PB_MISSING_JAVADOC_COMMENTS, JavaCore.ERROR);
			CompilationUnit result = (CompilationUnit)runConversion(sourceUnit, true);
			IProblem[] problems= result.getProblems();
			assertTrue(problems.length == 1);
			assertEquals("Invalid warning", "Javadoc: Missing tag for parameter a", problems[0].getMessage());
		} finally {
			project.setOptions(originalOptions);
		}
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46012
	 */
	public void test0501() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0501", "JavaEditor.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, false);
		assertNotNull(result);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46013
	 */
	public void test0502a() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0502", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// 'i' in initializer
		VariableDeclarationStatement variableDeclarationStatement = (VariableDeclarationStatement)getASTNode(unit, 0, 0, 0);
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) variableDeclarationStatement.fragments().get(0);
		IVariableBinding localBinding = fragment.resolveBinding();
		assertEquals("Unexpected key", "test0502/A/0/i", localBinding.getKey()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46013
	 */
	public void test0502b() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0502", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// 'j' in 'then' block in initializer
		IfStatement ifStatement = (IfStatement) getASTNode(unit, 0, 0, 1);
		Block block = (Block)ifStatement.getThenStatement();
		VariableDeclarationStatement variableDeclarationStatement = (VariableDeclarationStatement) block.statements().get(0);
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) variableDeclarationStatement.fragments().get(0);
		IVariableBinding localBinding = fragment.resolveBinding();
		assertEquals("Unexpected key", "test0502/A/0/0/j", localBinding.getKey()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46013
	 */
	public void test0502c() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0502", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// 'i' in 'foo()'
		VariableDeclarationStatement variableDeclarationStatement = (VariableDeclarationStatement)getASTNode(unit, 0, 1, 0);
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) variableDeclarationStatement.fragments().get(0);
		IVariableBinding localBinding = fragment.resolveBinding();
		assertEquals("Unexpected key", "test0502/A/voidfoo()/i", localBinding.getKey()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46013
	 */
	public void test0502d() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0502", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// 'j' in 'then' block in 'foo()'
		IfStatement ifStatement = (IfStatement) getASTNode(unit, 0, 1, 1);
		Block block = (Block)ifStatement.getThenStatement();
		VariableDeclarationStatement variableDeclarationStatement = (VariableDeclarationStatement) block.statements().get(0);
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) variableDeclarationStatement.fragments().get(0);
		IVariableBinding localBinding = fragment.resolveBinding();
		assertEquals("Unexpected key", "test0502/A/voidfoo()/0/j", localBinding.getKey()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46013
	 */
	public void test0502e() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0502", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// 'j' in 'else' block in 'foo()'
		IfStatement ifStatement = (IfStatement) getASTNode(unit, 0, 1, 1);
		Block block = (Block)ifStatement.getElseStatement();
		VariableDeclarationStatement variableDeclarationStatement = (VariableDeclarationStatement) block.statements().get(0);
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) variableDeclarationStatement.fragments().get(0);
		IVariableBinding localBinding = fragment.resolveBinding();
		assertEquals("Unexpected key", "test0502/A/voidfoo()/1/j", localBinding.getKey()); //$NON-NLS-1$
	}
	
	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46013
	 */
	public void test0502f() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0502", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// first 'new Object(){...}' in 'foo()'
		ExpressionStatement expressionStatement = (ExpressionStatement) getASTNode(unit, 0, 1, 2);
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expressionStatement.getExpression();
		AnonymousClassDeclaration anonymousClassDeclaration = classInstanceCreation.getAnonymousClassDeclaration();
		ITypeBinding typeBinding = anonymousClassDeclaration.resolveBinding();
		assertEquals("Unexpected key", "test0502/A$1", typeBinding.getKey()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46013
	 */
	public void test0502g() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0502", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// 'B' in 'foo()'
		TypeDeclarationStatement typeDeclarationStatement = (TypeDeclarationStatement) getASTNode(unit, 0, 1, 3);
		TypeDeclaration typeDeclaration = typeDeclarationStatement.getTypeDeclaration();
		ITypeBinding typeBinding = typeDeclaration.resolveBinding();
		assertEquals("Unexpected key", "test0502/A/voidfoo()/B", typeBinding.getKey()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46013
	 */
	public void test0502h() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0502", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// second 'new Object(){...}' in 'foo()'
		ExpressionStatement expressionStatement = (ExpressionStatement) getASTNode(unit, 0, 1, 4);
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expressionStatement.getExpression();
		AnonymousClassDeclaration anonymousClassDeclaration = classInstanceCreation.getAnonymousClassDeclaration();
		ITypeBinding typeBinding = anonymousClassDeclaration.resolveBinding();
		assertEquals("Unexpected key", "test0502/A$3", typeBinding.getKey()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46013
	 */
	public void test0502i() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0502", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// 'field' in 'B' in 'foo()'
		TypeDeclarationStatement typeDeclarationStatement = (TypeDeclarationStatement) getASTNode(unit, 0, 1, 3);
		TypeDeclaration typeDeclaration = typeDeclarationStatement.getTypeDeclaration();
		FieldDeclaration fieldDeclaration = typeDeclaration.getFields()[0];
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fieldDeclaration.fragments().get(0);
		IVariableBinding fieldBinding = fragment.resolveBinding();
		assertEquals("Unexpected key", "test0502/A/voidfoo()/B/field", fieldBinding.getKey()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46013
	 */
	public void test0502j() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0502", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// 'bar()' in 'B' in 'foo()'
		TypeDeclarationStatement typeDeclarationStatement = (TypeDeclarationStatement) getASTNode(unit, 0, 1, 3);
		TypeDeclaration typeDeclaration = typeDeclarationStatement.getTypeDeclaration();
		MethodDeclaration methodDeclaration = typeDeclaration.getMethods()[0];
		IMethodBinding methodBinding = methodDeclaration.resolveBinding();
		assertEquals("Unexpected key", "test0502/A/voidfoo()/B/voidbar()", methodBinding.getKey()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46057
	 */
	public void test0503a() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0503", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// top level type A
		TypeDeclaration type = (TypeDeclaration)getASTNode(unit, 0);
		ITypeBinding typeBinding = type.resolveBinding();
		assertEquals("Unexpected binary name", "test0503.A", typeBinding.getBinaryName()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46057
	 */
	public void test0503b() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0503", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// member type B in A
		TypeDeclaration type = (TypeDeclaration)getASTNode(unit, 0, 0);
		ITypeBinding typeBinding = type.resolveBinding();
		assertEquals("Unexpected binary name", "test0503.A$B", typeBinding.getBinaryName()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46057
	 */
	public void test0503c() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0503", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// local type E in foo() in A
		TypeDeclarationStatement typeDeclarationStatement = (TypeDeclarationStatement) getASTNode(unit, 0, 1, 0);
		TypeDeclaration typeDeclaration = typeDeclarationStatement.getTypeDeclaration();
		ITypeBinding typeBinding = typeDeclaration.resolveBinding();
		assertEquals("Unexpected binary name", "test0503.A$1$E", typeBinding.getBinaryName()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46057
	 */
	public void test0503d() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0503", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// anonymous type new Object() {...} in foo() in A
		ExpressionStatement expressionStatement = (ExpressionStatement) getASTNode(unit, 0, 1, 1);
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expressionStatement.getExpression();
		AnonymousClassDeclaration anonymousClassDeclaration = classInstanceCreation.getAnonymousClassDeclaration();
		ITypeBinding typeBinding = anonymousClassDeclaration.resolveBinding();
		assertEquals("Unexpected binary name", "test0503.A$2", typeBinding.getBinaryName()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46057
	 */
	public void test0503e() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0503", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// type F in anonymous type new Object() {...} in foo() in A
		ExpressionStatement expressionStatement = (ExpressionStatement) getASTNode(unit, 0, 1, 1);
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expressionStatement.getExpression();
		AnonymousClassDeclaration anonymousClassDeclaration = classInstanceCreation.getAnonymousClassDeclaration();
		TypeDeclaration type = (TypeDeclaration) anonymousClassDeclaration.bodyDeclarations().get(0);
		ITypeBinding typeBinding = type.resolveBinding();
		assertEquals("Unexpected binary name", "test0503.A$2$F", typeBinding.getBinaryName()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46057
	 */
	public void test0503f() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0503", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// local type C in bar() in B in A
		MethodDeclaration method = (MethodDeclaration) getASTNode(unit, 0, 0, 0);
		TypeDeclarationStatement typeDeclarationStatement = (TypeDeclarationStatement) method.getBody().statements().get(0);
		TypeDeclaration typeDeclaration = typeDeclarationStatement.getTypeDeclaration();
		ITypeBinding typeBinding = typeDeclaration.resolveBinding();
		assertEquals("Unexpected binary name", "test0503.A$1$C", typeBinding.getBinaryName()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46057
	 */
	public void test0503g() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0503", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// anonymous type new Object() {...} in bar() in B in A
		MethodDeclaration method = (MethodDeclaration) getASTNode(unit, 0, 0, 0);
		ExpressionStatement expressionStatement = (ExpressionStatement) method.getBody().statements().get(1);
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expressionStatement.getExpression();
		AnonymousClassDeclaration anonymousClassDeclaration = classInstanceCreation.getAnonymousClassDeclaration();
		ITypeBinding typeBinding = anonymousClassDeclaration.resolveBinding();
		assertEquals("Unexpected binary name", "test0503.A$1", typeBinding.getBinaryName()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46057
	 */
	public void test0503h() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0503", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// type D in anonymous type new Object() {...} in bar() in B in A
		MethodDeclaration method = (MethodDeclaration) getASTNode(unit, 0, 0, 0);
		ExpressionStatement expressionStatement = (ExpressionStatement) method.getBody().statements().get(1);
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expressionStatement.getExpression();
		AnonymousClassDeclaration anonymousClassDeclaration = classInstanceCreation.getAnonymousClassDeclaration();
		TypeDeclaration type = (TypeDeclaration) anonymousClassDeclaration.bodyDeclarations().get(0);
		ITypeBinding typeBinding = type.resolveBinding();
		assertEquals("Unexpected binary name", "test0503.A$1$D", typeBinding.getBinaryName()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=46057
	 */
	public void test0503i() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0503", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		CompilationUnit unit = (CompilationUnit)runConversion(sourceUnit, true);
		
		// unreachable type G in foo() in A
		IfStatement ifStatement = (IfStatement) getASTNode(unit, 0, 1, 2);
		Block block = (Block)ifStatement.getThenStatement();
		TypeDeclarationStatement typeDeclarationStatement = (TypeDeclarationStatement) block.statements().get(0);
		TypeDeclaration typeDeclaration = typeDeclarationStatement.getTypeDeclaration();
		ITypeBinding typeBinding = typeDeclaration.resolveBinding();
		assertEquals("Unexpected binary name", null, typeBinding.getBinaryName()); //$NON-NLS-1$
	}	

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=47396
	 */
	public void test0504() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0504", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 1, 0);
		assertNotNull(node);
		assertTrue("Not a constructor declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration declaration = (MethodDeclaration) node;
		assertTrue("A constructor", !declaration.isConstructor());
		checkSourceRange(declaration, "public method(final int parameter);", source);
	}

	/**
	 * http://bugs.eclipse.org/bugs/show_bug.cgi?id=47396
	 */
	public void test0505() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0505", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		assertTrue("not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 1, 0);
		assertNotNull(node);
		assertTrue("Not a constructor declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration declaration = (MethodDeclaration) node;
		assertTrue("A constructor", !declaration.isConstructor());
		checkSourceRange(declaration, "public method(final int parameter) {     }", source);
	}

	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=46699
	 */
	public void test0506() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0506", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		final CompilationUnit unit = (CompilationUnit) result;
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Wrong number of problems", 0, (unit).getProblems().length); //$NON-NLS-1$
		assertNotNull(node);
		assertTrue("Not an expression statement", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		assertTrue("Not a class instance creation", expressionStatement.getExpression().getNodeType() == ASTNode.CLASS_INSTANCE_CREATION); //$NON-NLS-1$
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expressionStatement.getExpression();
		IMethodBinding binding = classInstanceCreation.resolveConstructorBinding();
		assertFalse("is synthetic", binding.isSynthetic());
		assertTrue("is default constructor", binding.isDefaultConstructor());
		assertNull("Has a declaring node", unit.findDeclaringNode(binding));
	}

	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=46699
	 */
	public void test0507() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0507", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		final CompilationUnit unit = (CompilationUnit) result;
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Wrong number of problems", 0, (unit).getProblems().length); //$NON-NLS-1$
		assertNotNull(node);
		assertTrue("Not an expression statement", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		assertTrue("Not a class instance creation", expressionStatement.getExpression().getNodeType() == ASTNode.CLASS_INSTANCE_CREATION); //$NON-NLS-1$
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expressionStatement.getExpression();
		IMethodBinding binding = classInstanceCreation.resolveConstructorBinding();
		assertFalse("is synthetic", binding.isSynthetic());
		assertTrue("is default constructor", binding.isDefaultConstructor());
		assertNull("Has a declaring node", unit.findDeclaringNode(binding));
	}

	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=46699
	 */
	public void test0508() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0508", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		final CompilationUnit unit = (CompilationUnit) result;
		ASTNode node = getASTNode(unit, 0, 1, 0);
		assertEquals("Wrong number of problems", 0, (unit).getProblems().length); //$NON-NLS-1$
		assertNotNull(node);
		assertTrue("Not an expression statement", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		assertTrue("Not a class instance creation", expressionStatement.getExpression().getNodeType() == ASTNode.CLASS_INSTANCE_CREATION); //$NON-NLS-1$
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expressionStatement.getExpression();
		IMethodBinding binding = classInstanceCreation.resolveConstructorBinding();
		assertFalse("is synthetic", binding.isSynthetic());
		assertTrue("not a default constructor", !binding.isDefaultConstructor());
		assertNotNull("Has no declaring node", unit.findDeclaringNode(binding));
	}

	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=46699
	 */
	public void test0509() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0509", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		final CompilationUnit unit = (CompilationUnit) result;
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Wrong number of problems", 0, (unit).getProblems().length); //$NON-NLS-1$
		assertNotNull(node);
		assertTrue("Not an expression statement", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		assertTrue("Not a class instance creation", expressionStatement.getExpression().getNodeType() == ASTNode.CLASS_INSTANCE_CREATION); //$NON-NLS-1$
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expressionStatement.getExpression();
		IMethodBinding binding = classInstanceCreation.resolveConstructorBinding();
		assertFalse("is synthetic", binding.isSynthetic());
		assertTrue("not a default constructor", !binding.isDefaultConstructor());
		assertNull("Has a declaring node", unit.findDeclaringNode(binding));
	}

	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=46699
	 */
	public void test0510() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0510", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		final CompilationUnit unit = (CompilationUnit) result;
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Wrong number of problems", 0, (unit).getProblems().length); //$NON-NLS-1$
		assertNotNull(node);
		assertTrue("Not an expression statement", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		assertTrue("Not a class instance creation", expressionStatement.getExpression().getNodeType() == ASTNode.CLASS_INSTANCE_CREATION); //$NON-NLS-1$
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expressionStatement.getExpression();
		IMethodBinding binding = classInstanceCreation.resolveConstructorBinding();
		assertFalse("is synthetic", binding.isSynthetic());
		assertFalse("is default constructor", binding.isDefaultConstructor());
		assertNull("Has a declaring node", unit.findDeclaringNode(binding));
	}

	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=46699
	 */
	public void test0511() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0511", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		final CompilationUnit unit = (CompilationUnit) result;
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertEquals("Wrong number of problems", 0, (unit).getProblems().length); //$NON-NLS-1$
		assertNotNull(node);
		assertTrue("Not an expression statement", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		assertTrue("Not a class instance creation", expressionStatement.getExpression().getNodeType() == ASTNode.CLASS_INSTANCE_CREATION); //$NON-NLS-1$
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expressionStatement.getExpression();
		IMethodBinding binding = classInstanceCreation.resolveConstructorBinding();
		assertFalse("is synthetic", binding.isSynthetic());
		assertFalse("is default constructor", binding.isDefaultConstructor());
		assertNull("Has a declaring node", unit.findDeclaringNode(binding));
	}

	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=47326
	 */
	public void test0512() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0512", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		final CompilationUnit unit = (CompilationUnit) result;
		ASTNode node = getASTNode(unit, 0, 0);
		assertEquals("Wrong number of problems", 2, unit.getProblems().length); //$NON-NLS-1$
		assertNotNull(node);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration declaration = (MethodDeclaration) node;
		assertTrue("Not a constructor", declaration.isConstructor());
		checkSourceRange(declaration, "public A();", source);
	}
	
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=49429
	 */
	public void test0513() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0513", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$
	}
	
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=48502
	 */
	public void _test0514() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0514", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$
	}
	
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=49204
	 */
	public void test0515() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0515", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a if statement", node.getNodeType() == ASTNode.IF_STATEMENT);
		IfStatement ifStatement = (IfStatement) node;
		assertTrue("not an empty statement", ifStatement.getThenStatement().getNodeType() == ASTNode.EMPTY_STATEMENT);
		checkSourceRange(ifStatement.getThenStatement(), ";", source);
		Statement statement = ifStatement.getElseStatement();
		assertTrue("not a if statement", statement.getNodeType() == ASTNode.IF_STATEMENT);
		ifStatement = (IfStatement) statement;
		assertTrue("not an empty statement", ifStatement.getThenStatement().getNodeType() == ASTNode.EMPTY_STATEMENT);
		checkSourceRange(ifStatement.getThenStatement(), ";", source);
		Statement statement2 = ifStatement.getElseStatement();
		assertTrue("not an empty statement", statement2.getNodeType() == ASTNode.EMPTY_STATEMENT);
		checkSourceRange(statement2, ";", source);
	}
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=48489
	 */
	public void test0516() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0516", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, false);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION);
		MethodDeclaration declaration = (MethodDeclaration) node;
		ASTParser parser = ASTParser.newParser(AST.LEVEL_2_0);
		parser.setKind(ASTParser.K_CLASS_BODY_DECLARATIONS);
		parser.setSource(source);
		parser.setSourceRange(declaration.getStartPosition(), declaration.getLength());
		parser.setCompilerOptions(JavaCore.getOptions());
		ASTNode result2 = parser.createAST(null);
		assertNotNull("No node", result2);
		assertTrue("not a type declaration", result2.getNodeType() == ASTNode.TYPE_DECLARATION);
		TypeDeclaration typeDeclaration = (TypeDeclaration) result2;
		List bodyDeclarations = typeDeclaration.bodyDeclarations();
		assertEquals("wrong size", 1, bodyDeclarations.size());
		BodyDeclaration bodyDeclaration = (BodyDeclaration) bodyDeclarations.get(0);
		assertTrue(declaration.subtreeMatch(new ASTMatcher(), bodyDeclaration));
		ASTNode root = bodyDeclaration.getRoot();
		assertNotNull("No root", root);
		assertTrue("not a compilation unit", root.getNodeType() == ASTNode.COMPILATION_UNIT);
		CompilationUnit compilationUnit = (CompilationUnit) root;
		assertEquals("wrong problem size", 0, compilationUnit.getProblems().length);
		assertNotNull("No comments", compilationUnit.getCommentList());
		assertEquals("Wrong size", 3, compilationUnit.getCommentList().size());
	}	
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=48489
	 */
	public void test0517() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0517", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, false);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$
		assertNotNull("No comments", unit.getCommentList());
		assertEquals("Wrong size", 3, unit.getCommentList().size());
		ASTNode node = getASTNode(unit, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION);
		FieldDeclaration declaration = (FieldDeclaration) node;
		ASTParser parser = ASTParser.newParser(AST.LEVEL_2_0);
		parser.setKind(ASTParser.K_CLASS_BODY_DECLARATIONS);
		parser.setSource(source);
		parser.setSourceRange(declaration.getStartPosition(), declaration.getLength());
		parser.setCompilerOptions(JavaCore.getOptions());
		ASTNode result2 = parser.createAST(null);
		assertNotNull("No node", result2);
		assertTrue("not a type declaration", result2.getNodeType() == ASTNode.TYPE_DECLARATION);
		TypeDeclaration typeDeclaration = (TypeDeclaration) result2;
		List bodyDeclarations = typeDeclaration.bodyDeclarations();
		assertEquals("wrong size", 1, bodyDeclarations.size());
		BodyDeclaration bodyDeclaration = (BodyDeclaration) bodyDeclarations.get(0);
		assertTrue(declaration.subtreeMatch(new ASTMatcher(), bodyDeclaration));
		ASTNode root = bodyDeclaration.getRoot();
		assertNotNull("No root", root);
		assertTrue("not a compilation unit", root.getNodeType() == ASTNode.COMPILATION_UNIT);
		CompilationUnit compilationUnit = (CompilationUnit) root;
		assertEquals("wrong problem size", 0, compilationUnit.getProblems().length);
		assertNotNull("No comments", compilationUnit.getCommentList());
		assertEquals("Wrong size", 2, compilationUnit.getCommentList().size());
	}
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=48489
	 */
	public void test0518() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0518", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, false);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not an initializer", node.getNodeType() == ASTNode.INITIALIZER);
		Initializer declaration = (Initializer) node;
		ASTParser parser = ASTParser.newParser(AST.LEVEL_2_0);
		parser.setKind(ASTParser.K_CLASS_BODY_DECLARATIONS);
		parser.setSource(source);
		parser.setSourceRange(declaration.getStartPosition(), declaration.getLength());
		parser.setCompilerOptions(JavaCore.getOptions());
		ASTNode result2 = parser.createAST(null);
		assertNotNull("No node", result2);
		assertTrue("not a type declaration", result2.getNodeType() == ASTNode.TYPE_DECLARATION);
		TypeDeclaration typeDeclaration = (TypeDeclaration) result2;
		List bodyDeclarations = typeDeclaration.bodyDeclarations();
		assertEquals("wrong size", 1, bodyDeclarations.size());
		BodyDeclaration bodyDeclaration = (BodyDeclaration) bodyDeclarations.get(0);
		assertTrue(declaration.subtreeMatch(new ASTMatcher(), bodyDeclaration));
		ASTNode root = bodyDeclaration.getRoot();
		assertNotNull("No root", root);
		assertTrue("not a compilation unit", root.getNodeType() == ASTNode.COMPILATION_UNIT);
		CompilationUnit compilationUnit = (CompilationUnit) root;
		assertEquals("wrong problem size", 0, compilationUnit.getProblems().length);
		assertNotNull("No comments", compilationUnit.getCommentList());
		assertEquals("Wrong size", 3, compilationUnit.getCommentList().size());
	}
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=48489
	 */
	public void test0519() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0519", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, false);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$
		assertNotNull("No comments", unit.getCommentList());
		assertEquals("Wrong size", 2, unit.getCommentList().size());
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertNotNull("No node", node);
		ASTNode statement = node;
		ASTParser parser = ASTParser.newParser(AST.LEVEL_2_0);
		parser.setKind(ASTParser.K_STATEMENTS);
		parser.setSource(source);
		parser.setSourceRange(statement.getStartPosition(), statement.getLength());
		parser.setCompilerOptions(JavaCore.getOptions());
		ASTNode result2 = parser.createAST(null);
		assertNotNull("No node", result2);
		assertTrue("not a block", result2.getNodeType() == ASTNode.BLOCK);
		Block block = (Block) result2;
		List statements = block.statements();
		assertEquals("wrong size", 1, statements.size());
		Statement statement2 = (Statement) statements.get(0);
		assertTrue(statement.subtreeMatch(new ASTMatcher(), statement2));
		ASTNode root = statement2.getRoot();
		assertNotNull("No root", root);
		assertTrue("not a compilation unit", root.getNodeType() == ASTNode.COMPILATION_UNIT);
		CompilationUnit compilationUnit = (CompilationUnit) root;
		assertEquals("wrong problem size", 0, compilationUnit.getProblems().length);
		assertNotNull("No comments", compilationUnit.getCommentList());
		assertEquals("Wrong size", 1, compilationUnit.getCommentList().size());
	}
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=48489
	 */
	public void test0520() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0520", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, false);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$
		assertNotNull("No comments", unit.getCommentList());
		assertEquals("Wrong size", 2, unit.getCommentList().size());
		ASTNode node = getASTNode(unit, 0, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a block", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT);
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		ASTParser parser = ASTParser.newParser(AST.LEVEL_2_0);
		parser.setKind(ASTParser.K_EXPRESSION);
		parser.setSource(source);
		parser.setSourceRange(expression.getStartPosition(), expression.getLength());
		parser.setCompilerOptions(JavaCore.getOptions());
		ASTNode result2 = parser.createAST(null);
		assertNotNull("No node", result2);
		assertTrue("not a method invocation", result2.getNodeType() == ASTNode.METHOD_INVOCATION);
		assertTrue(expression.subtreeMatch(new ASTMatcher(), result2));
		ASTNode root = result2.getRoot();
		assertNotNull("No root", root);
		assertTrue("not a compilation unit", root.getNodeType() == ASTNode.COMPILATION_UNIT);
		CompilationUnit compilationUnit = (CompilationUnit) root;
		assertEquals("wrong problem size", 0, compilationUnit.getProblems().length);
		assertNotNull("No comments", compilationUnit.getCommentList());
		assertEquals("Wrong size", 1, compilationUnit.getCommentList().size());
	}
	/*
	 * Ensure an OperationCanceledException is correcly thrown when progress monitor is canceled
	 */
	public void test0521() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0521", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		
		// count the number of time isCanceled() is called when converting this source unit
		WorkingCopyOwner owner = new WorkingCopyOwner() {};
		class Counter implements IProgressMonitor {
			int count = 0;
			public void beginTask(String name, int totalWork) {}
			public void done() {}
			public void internalWorked(double work) {}
			public boolean isCanceled() {
				count++;
				return false;
			}
			public void setCanceled(boolean value) {}
			public void setTaskName(String name) {}
			public void subTask(String name) {}
			public void worked(int work) {}
		}
		Counter counter = new Counter();
		ASTParser parser = ASTParser.newParser(AST.LEVEL_2_0);
		parser.setSource(sourceUnit);
		parser.setResolveBindings(true);
		parser.setWorkingCopyOwner(owner);
		parser.createAST(counter);
		
		// throw an OperatonCanceledException at each point isCanceled() is called
		class Canceler implements IProgressMonitor {
			int count;
			Canceler(int count) {
				this.count = count;
			}
			public void beginTask(String name, int totalWork) {}
			public void done() {}
			public void internalWorked(double work) {}
			public boolean isCanceled() {
				return --count < 0;
			}
			public void setCanceled(boolean value) {}
			public void setTaskName(String name) {}
			public void subTask(String name) {}
			public void worked(int work) {}
		}
		for (int i = 0; i < counter.count; i++) {
			boolean gotException = false;
			try {
				parser = ASTParser.newParser(AST.LEVEL_2_0);
				parser.setSource(sourceUnit);
				parser.setResolveBindings(true);
				parser.setWorkingCopyOwner(owner);
				parser.createAST(new Canceler(i));
			} catch (OperationCanceledException e) {
				gotException = true;
			}
			assertTrue("Should get an OperationCanceledException (" + i + ")", gotException);
		}
		
		// last should not throw an OperationCanceledException
		parser = ASTParser.newParser(AST.LEVEL_2_0);
		parser.setSource(sourceUnit);
		parser.setResolveBindings(true);
		parser.setWorkingCopyOwner(owner);
		parser.createAST(new Canceler(counter.count));
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=48292
	 */
	public void test0522() throws JavaModelException {
		IClassFile classFile = getClassFile("Converter" , "bins", "test0522", "Test.class"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		assertNotNull(classFile);
		assertNotNull(classFile.getSource());
		IType type = classFile.getType();
		assertNotNull(type);
		IMethod[] methods = type.getMethods();
		assertNotNull(methods);
		assertEquals("wrong size", 2, methods.length);
		IMethod method = methods[1];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(classFile, sourceRange.getOffset() + sourceRange.getLength() / 2, true);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 1, 0);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertEquals("wrong name", "test", methodDeclaration.getName().getIdentifier());
		IMethodBinding methodBinding = methodDeclaration.resolveBinding();
		assertNotNull(methodBinding);
		List statements = ((MethodDeclaration) node).getBody().statements();
		assertEquals("wrong size", 2, statements.size());
		ASTNode node2 = (ASTNode) statements.get(1);
		assertNotNull(node2);
		assertTrue("Not an expression statement", node2.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node2;
		Expression expression = expressionStatement.getExpression();
		assertTrue("Not a method invocation", expression.getNodeType() == ASTNode.METHOD_INVOCATION); //$NON-NLS-1$
		MethodInvocation methodInvocation = (MethodInvocation) expression;
		Expression expression2 = methodInvocation.getExpression();
		assertTrue("Not a simple name", expression2.getNodeType() == ASTNode.SIMPLE_NAME); //$NON-NLS-1$
		SimpleName simpleName = (SimpleName) expression2;
		IBinding binding  = simpleName.resolveBinding();
		assertNotNull("No binding", binding); //$NON-NLS-1$
		assertTrue("wrong type", binding.getKind() == IBinding.VARIABLE); //$NON-NLS-1$
		IVariableBinding variableBinding = (IVariableBinding) binding;
		assertEquals("Wrong name", "a", variableBinding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
		SimpleName simpleName2 = methodInvocation.getName();
		assertEquals("Wrong name", "clone", simpleName2.getIdentifier()); //$NON-NLS-1$ //$NON-NLS-2$
		IBinding binding2 = simpleName2.resolveBinding();
		assertNotNull("no binding2", binding2); //$NON-NLS-1$
		assertTrue("Wrong type", binding2.getKind() == IBinding.METHOD); //$NON-NLS-1$
		IMethodBinding methodBinding2 = (IMethodBinding) binding2;
		assertEquals("Wrong name", "clone", methodBinding2.getName()); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=48292
	 */
	public void test0523() throws JavaModelException {
		IClassFile classFile = getClassFile("Converter" , "bins", "test0523", "Test.class"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		assertNotNull(classFile);
		assertNotNull(classFile.getSource());
		IType type = classFile.getType();
		assertNotNull(type);
		IMethod[] methods = type.getMethods();
		assertNotNull(methods);
		assertEquals("wrong size", 2, methods.length);
		IMethod method = methods[1];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(classFile, sourceRange.getOffset() + sourceRange.getLength() / 2, false);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 1, 0);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertEquals("wrong name", "test", methodDeclaration.getName().getIdentifier());
		List statements = ((MethodDeclaration) node).getBody().statements();
		assertEquals("wrong size", 2, statements.size());
		ASTNode node2 = (ASTNode) statements.get(1);
		assertNotNull(node2);
		assertTrue("Not an expression statement", node2.getNodeType() == ASTNode.EXPRESSION_STATEMENT); //$NON-NLS-1$
		ExpressionStatement expressionStatement = (ExpressionStatement) node2;
		Expression expression = expressionStatement.getExpression();
		assertTrue("Not a method invocation", expression.getNodeType() == ASTNode.METHOD_INVOCATION); //$NON-NLS-1$
		MethodInvocation methodInvocation = (MethodInvocation) expression;
		Expression expression2 = methodInvocation.getExpression();
		assertTrue("Not a simple name", expression2.getNodeType() == ASTNode.SIMPLE_NAME); //$NON-NLS-1$
		SimpleName simpleName = (SimpleName) expression2;
		IBinding binding  = simpleName.resolveBinding();
		assertNull("No binding", binding); //$NON-NLS-1$
		SimpleName simpleName2 = methodInvocation.getName();
		assertEquals("Wrong name", "clone", simpleName2.getIdentifier()); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=48292
	 */
	public void test0524() throws JavaModelException {
		IClassFile classFile = getClassFile("Converter" , "bins", "test0524", "A.class"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		assertNotNull(classFile);
		assertNotNull(classFile.getSource());
		IType type = classFile.getType();
		assertNotNull(type);
		IMethod[] methods = type.getMethods();
		assertNotNull(methods);
		assertEquals("wrong size", 2, methods.length);
		IMethod method = methods[1];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(classFile, sourceRange.getOffset() + sourceRange.getLength() / 2, false);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertEquals("wrong name", "test", methodDeclaration.getName().getIdentifier());
		List statements = ((MethodDeclaration) node).getBody().statements();
		assertEquals("wrong size", 1, statements.size());
		ASTNode node2 = (ASTNode) statements.get(0);
		assertNotNull(node2);
		assertTrue("Not an variable declaration statement", node2.getNodeType() == ASTNode.VARIABLE_DECLARATION_STATEMENT); //$NON-NLS-1$
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=48292
	 */
	public void test0525() throws JavaModelException {
		IClassFile classFile = getClassFile("Converter" , "bins", "test0525", "A.class"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		assertNotNull(classFile);
		assertNotNull(classFile.getSource());
		IType type = classFile.getType();
		assertNotNull(type);
		IMethod[] methods = type.getMethods();
		assertNotNull(methods);
		assertEquals("wrong size", 2, methods.length);
		IMethod method = methods[1];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(classFile, sourceRange.getOffset() + sourceRange.getLength() / 2, true);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertEquals("wrong name", "test", methodDeclaration.getName().getIdentifier());
		List statements = ((MethodDeclaration) node).getBody().statements();
		assertEquals("wrong size", 1, statements.size());
		ASTNode node2 = (ASTNode) statements.get(0);
		assertNotNull(node2);
		assertTrue("Not an variable declaration statement", node2.getNodeType() == ASTNode.VARIABLE_DECLARATION_STATEMENT); //$NON-NLS-1$
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node2;
		List fragments = statement.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertTrue("Not a class instance creation", expression.getNodeType() == ASTNode.CLASS_INSTANCE_CREATION); //$NON-NLS-1$
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expression;
		ITypeBinding typeBinding = classInstanceCreation.resolveTypeBinding();
		assertNotNull(typeBinding);
	}
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=48292
	 */
	public void test0526() throws JavaModelException {
		IClassFile classFile = getClassFile("Converter" , "bins", "test0526", "A.class"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		assertNotNull(classFile);
		assertNotNull(classFile.getSource());
		IType type = classFile.getType();
		assertNotNull(type);
		IType[] memberTypes = type.getTypes();
		assertNotNull(memberTypes);
		assertEquals("wrong size", 1, memberTypes.length);
		IType memberType = memberTypes[0];
		IMethod[] methods = memberType.getMethods();
		assertEquals("wrong size", 2, methods.length);
		IMethod method = methods[1];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(classFile, sourceRange.getOffset() + sourceRange.getLength() / 2, true);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0, 0);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertEquals("wrong name", "test", methodDeclaration.getName().getIdentifier());
		List statements = ((MethodDeclaration) node).getBody().statements();
		assertEquals("wrong size", 1, statements.size());
		ASTNode node2 = (ASTNode) statements.get(0);
		assertNotNull(node2);
		assertTrue("Not an variable declaration statement", node2.getNodeType() == ASTNode.VARIABLE_DECLARATION_STATEMENT); //$NON-NLS-1$
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node2;
		List fragments = statement.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertTrue("Not a class instance creation", expression.getNodeType() == ASTNode.CLASS_INSTANCE_CREATION); //$NON-NLS-1$
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expression;
		ITypeBinding typeBinding = classInstanceCreation.resolveTypeBinding();
		assertNotNull(typeBinding);
		assertTrue(typeBinding.isAnonymous());
		assertEquals("Wrong name", "", typeBinding.getName());
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=48292
	 */
	public void test0527() throws JavaModelException {
		IClassFile classFile = getClassFile("Converter" , "bins", "test0527", "A.class"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		assertNotNull(classFile);
		assertNotNull(classFile.getSource());
		IType type = classFile.getType();
		assertNotNull(type);
		IMethod[] methods = type.getMethods();
		assertEquals("wrong size", 1, methods.length);
		IMethod method = methods[0];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(classFile, sourceRange.getOffset() + sourceRange.getLength() / 2, true);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		assertEquals("wrong name", "A", methodDeclaration.getName().getIdentifier());
		assertTrue("Not a constructor", methodDeclaration.isConstructor());
		IBinding binding = methodDeclaration.getName().resolveBinding();
		assertNotNull(binding);
		assertEquals("Wrong type", IBinding.METHOD, binding.getKind());
		List statements = ((MethodDeclaration) node).getBody().statements();
		assertEquals("wrong size", 1, statements.size());
		ASTNode node2 = (ASTNode) statements.get(0);
		assertNotNull(node2);
		assertTrue("Not an variable declaration statement", node2.getNodeType() == ASTNode.VARIABLE_DECLARATION_STATEMENT); //$NON-NLS-1$
		VariableDeclarationStatement statement = (VariableDeclarationStatement) node2;
		List fragments = statement.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertTrue("Not a class instance creation", expression.getNodeType() == ASTNode.CLASS_INSTANCE_CREATION); //$NON-NLS-1$
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expression;
		ITypeBinding typeBinding = classInstanceCreation.resolveTypeBinding();
		assertNotNull(typeBinding);
		assertTrue(typeBinding.isAnonymous());
		assertEquals("Wrong name", "", typeBinding.getName());
	}
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=48292
	 */
	public void test0528() throws JavaModelException {
		IClassFile classFile = getClassFile("Converter" , "bins", "test0528", "A.class"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		assertNotNull(classFile);
		assertNotNull(classFile.getSource());
		IType type = classFile.getType();
		IType[] memberTypes = type.getTypes();
		assertNotNull(memberTypes);
		assertEquals("wrong size", 1, memberTypes.length);
		IType memberType = memberTypes[0];
		ISourceRange sourceRange = memberType.getSourceRange(); 
		ASTNode result = runConversion(classFile, sourceRange.getOffset() + sourceRange.getLength() / 2, true);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a type declaration", node.getNodeType() == ASTNode.TYPE_DECLARATION); //$NON-NLS-1$
		TypeDeclaration typeDeclaration = (TypeDeclaration) node;
		assertEquals("wrong name", "B", typeDeclaration.getName().getIdentifier());
		List bodyDeclarations = typeDeclaration.bodyDeclarations();
		assertEquals("Wrong size", 1, bodyDeclarations.size());
		BodyDeclaration bodyDeclaration = (BodyDeclaration) bodyDeclarations.get(0);
		assertTrue("Not a method declaration", bodyDeclaration.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) bodyDeclaration;
		Block block = methodDeclaration.getBody();
		List statements = block.statements();
		assertEquals("Wrong size", 1, statements.size());
	}
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=48292
	 */
	public void test0529() throws JavaModelException {
		IClassFile classFile = getClassFile("Converter" , "bins", "test0529", "A.class"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		assertNotNull(classFile);
		assertNotNull(classFile.getSource());
		IType type = classFile.getType();
		IType[] memberTypes = type.getTypes();
		assertNotNull(memberTypes);
		assertEquals("wrong size", 1, memberTypes.length);
		IType memberType = memberTypes[0];
		ISourceRange sourceRange = memberType.getSourceRange(); 
		ASTNode result = runConversion(classFile, sourceRange.getOffset() + sourceRange.getLength() / 2, false);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a type declaration", node.getNodeType() == ASTNode.TYPE_DECLARATION); //$NON-NLS-1$
		TypeDeclaration typeDeclaration = (TypeDeclaration) node;
		assertEquals("wrong name", "B", typeDeclaration.getName().getIdentifier());
		List bodyDeclarations = typeDeclaration.bodyDeclarations();
		assertEquals("Wrong size", 1, bodyDeclarations.size());
		BodyDeclaration bodyDeclaration = (BodyDeclaration) bodyDeclarations.get(0);
		assertTrue("Not a method declaration", bodyDeclaration.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) bodyDeclaration;
		Block block = methodDeclaration.getBody();
		List statements = block.statements();
		assertEquals("Wrong size", 1, statements.size());
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=48292
	 */
	public void test0530() throws JavaModelException {
		IClassFile classFile = getClassFile("Converter" , "bins", "test0530", "A.class"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		assertNotNull(classFile);
		assertNotNull(classFile.getSource());
		IType type = classFile.getType();
		IMethod[] methods = type.getMethods();
		assertEquals("wrong size", 3, methods.length);
		IMethod method = methods[2];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(classFile, sourceRange.getOffset() + sourceRange.getLength() / 2, false);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
		ASTNode node = getASTNode((CompilationUnit) result, 0, 2);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		Block block = methodDeclaration.getBody();
		List statements = block.statements();
		assertEquals("Wrong size", 2, statements.size());

		node = getASTNode((CompilationUnit) result, 0, 1);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		methodDeclaration = (MethodDeclaration) node;
		block = methodDeclaration.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
	}
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=48292
	 */
	public void test0531() throws JavaModelException {
		IClassFile classFile = getClassFile("Converter" , "bins", "test0531", "A.class"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		assertNotNull(classFile);
		assertNotNull(classFile.getSource());
		IType type = classFile.getType();
		IMethod[] methods = type.getMethods();
		assertEquals("wrong size", 5, methods.length);
		IMethod method = methods[3];
		ISourceRange sourceRange = method.getSourceRange(); 
		ASTNode result = runConversion(classFile, sourceRange.getOffset() + sourceRange.getLength() / 2, false);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$

		ASTNode node = getASTNode((CompilationUnit) result, 0, 5);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		Block block = methodDeclaration.getBody();
		List statements = block.statements();
		assertEquals("Wrong size", 2, statements.size());

		node = getASTNode((CompilationUnit) result, 0, 4);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		methodDeclaration = (MethodDeclaration) node;
		block = methodDeclaration.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
		
		node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION); //$NON-NLS-1$
		FieldDeclaration fieldDeclaration = (FieldDeclaration) node;
		List fragments = fieldDeclaration.fragments();
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertEquals("Wrong name", "field", fragment.getName().getIdentifier());
		assertNotNull("No initializer", expression);

		node = getASTNode((CompilationUnit) result, 0, 1);
		assertTrue("Not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION); //$NON-NLS-1$
		fieldDeclaration = (FieldDeclaration) node;
		fragments = fieldDeclaration.fragments();
		fragment = (VariableDeclarationFragment) fragments.get(0);
		expression = fragment.getInitializer();
		assertEquals("Wrong name", "i", fragment.getName().getIdentifier());
		assertNotNull("No initializer", expression);

		node = getASTNode((CompilationUnit) result, 0, 2);
		assertTrue("Not an initializer", node.getNodeType() == ASTNode.INITIALIZER); //$NON-NLS-1$
		Initializer initializer = (Initializer) node;
		assertEquals("Not static", Modifier.NONE, initializer.getModifiers());
		block = initializer.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
		
		node = getASTNode((CompilationUnit) result, 0, 3);
		assertTrue("Not an initializer", node.getNodeType() == ASTNode.INITIALIZER); //$NON-NLS-1$
		initializer = (Initializer) node;
		assertEquals("Not static", Modifier.STATIC, initializer.getModifiers());
		block = initializer.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
		
		node = getASTNode((CompilationUnit) result, 0, 6);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		methodDeclaration = (MethodDeclaration) node;
		block = methodDeclaration.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
	}

	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=48292
	 */
	public void test0532() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter" , "src", "test0488", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		IType[] types = sourceUnit.getTypes();
		assertNotNull(types);
		assertEquals("wrong size", 1, types.length);
		IType type = types[0];
		IInitializer[] initializers = type.getInitializers();
		assertEquals("wrong size", 2, initializers.length);
		IInitializer init = initializers[1];
		ISourceRange sourceRange = init.getSourceRange(); 
		int position = sourceRange.getOffset() + sourceRange.getLength() / 2;

		IClassFile classFile = getClassFile("Converter" , "bins", "test0532", "A.class"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		assertNotNull(classFile);
		assertNotNull(classFile.getSource());
		type = classFile.getType();
		initializers = type.getInitializers();
		assertEquals("wrong size", 0, initializers.length);
		ASTNode result = runConversion(classFile, position, false);
		assertNotNull(result);
		assertTrue("Not a compilation unit", result.getNodeType() == ASTNode.COMPILATION_UNIT); //$NON-NLS-1$
	
		ASTNode node = getASTNode((CompilationUnit) result, 0, 5);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		MethodDeclaration methodDeclaration = (MethodDeclaration) node;
		Block block = methodDeclaration.getBody();
		List statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
	
		node = getASTNode((CompilationUnit) result, 0, 4);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		methodDeclaration = (MethodDeclaration) node;
		block = methodDeclaration.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
		
		node = getASTNode((CompilationUnit) result, 0, 0);
		assertTrue("Not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION); //$NON-NLS-1$
		FieldDeclaration fieldDeclaration = (FieldDeclaration) node;
		List fragments = fieldDeclaration.fragments();
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		Expression expression = fragment.getInitializer();
		assertEquals("Wrong name", "field", fragment.getName().getIdentifier());
		assertNotNull("No initializer", expression);
	
		node = getASTNode((CompilationUnit) result, 0, 1);
		assertTrue("Not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION); //$NON-NLS-1$
		fieldDeclaration = (FieldDeclaration) node;
		fragments = fieldDeclaration.fragments();
		fragment = (VariableDeclarationFragment) fragments.get(0);
		expression = fragment.getInitializer();
		assertEquals("Wrong name", "i", fragment.getName().getIdentifier());
		assertNotNull("No initializer", expression);
	
		node = getASTNode((CompilationUnit) result, 0, 2);
		assertTrue("Not an initializer", node.getNodeType() == ASTNode.INITIALIZER); //$NON-NLS-1$
		Initializer initializer = (Initializer) node;
		assertEquals("Not static", Modifier.NONE, initializer.getModifiers());
		block = initializer.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
		
		node = getASTNode((CompilationUnit) result, 0, 3);
		assertTrue("Not an initializer", node.getNodeType() == ASTNode.INITIALIZER); //$NON-NLS-1$
		initializer = (Initializer) node;
		assertEquals("Not static", Modifier.STATIC, initializer.getModifiers());
		block = initializer.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 1, statements.size());
		
		node = getASTNode((CompilationUnit) result, 0, 6);
		assertTrue("Not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION); //$NON-NLS-1$
		methodDeclaration = (MethodDeclaration) node;
		block = methodDeclaration.getBody();
		statements = block.statements();
		assertEquals("Wrong size", 0, statements.size());
	}
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=48489
	 */
	public void test0533() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0533", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, false);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a method declaration", node.getNodeType() == ASTNode.METHOD_DECLARATION);
		MethodDeclaration declaration = (MethodDeclaration) node;
		ASTParser parser = ASTParser.newParser(AST.LEVEL_2_0);
		parser.setKind(ASTParser.K_CLASS_BODY_DECLARATIONS);
		parser.setSource(source);
		parser.setSourceRange(declaration.getStartPosition(), declaration.getLength());
		parser.setCompilerOptions(JavaCore.getOptions());
		ASTNode result2 = parser.createAST(null);
		assertNotNull("No node", result2);
		assertTrue("not a compilation unit", result2.getNodeType() == ASTNode.COMPILATION_UNIT);
		CompilationUnit compilationUnit = (CompilationUnit) result2;
		assertEquals("wrong problem size", 1, compilationUnit.getProblems().length);
	}	
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=48489
	 */
	public void test0534() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0534", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, false);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION);
		FieldDeclaration declaration = (FieldDeclaration) node;
		ASTParser parser = ASTParser.newParser(AST.LEVEL_2_0);
		parser.setKind(ASTParser.K_CLASS_BODY_DECLARATIONS);
		parser.setSource(source);
		parser.setSourceRange(declaration.getStartPosition(), declaration.getLength());
		parser.setCompilerOptions(JavaCore.getOptions());
		ASTNode result2 = parser.createAST(null);
		assertNotNull("No node", result2);
		assertTrue("not a compilation unit", result2.getNodeType() == ASTNode.COMPILATION_UNIT);
		CompilationUnit compilationUnit = (CompilationUnit) result2;
		assertEquals("wrong problem size", 1, compilationUnit.getProblems().length);
	}
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=48489
	 */
	public void test0535() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0535", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, false);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 1, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0, 0);
		assertNotNull("No node", node);
		assertTrue("not an initializer", node.getNodeType() == ASTNode.INITIALIZER);
		Initializer declaration = (Initializer) node;
		ASTParser parser = ASTParser.newParser(AST.LEVEL_2_0);
		parser.setKind(ASTParser.K_CLASS_BODY_DECLARATIONS);
		parser.setSource(source);
		parser.setSourceRange(declaration.getStartPosition(), declaration.getLength());
		parser.setCompilerOptions(JavaCore.getOptions());
		ASTNode result2 = parser.createAST(null);
		assertNotNull("No node", result2);
		assertTrue("not a compilation unit", result2.getNodeType() == ASTNode.COMPILATION_UNIT);
		CompilationUnit compilationUnit = (CompilationUnit) result2;
		assertEquals("wrong problem size", 1, compilationUnit.getProblems().length);
	}
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=47396
	 */
	public void test0536() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0536", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, false);
		assertNotNull("No compilation unit", result);
	}
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=51089
	 */
	public void test0537a() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0537", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, false);
		assertNotNull("No compilation unit", result);
	}
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=51089
	 */
	public void test0537b() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0537", "B.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, false);
		assertNotNull("No compilation unit", result);
	}
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=51089
	 */
	public void test0537c() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0537", "C.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, false);
		assertNotNull("No compilation unit", result);
	}
	/*
	 * Ensures that an AST can be created during reconcile.
	 */
	public void test0538a() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0538", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		try {
			sourceUnit.becomeWorkingCopy(null, null);
			sourceUnit.getBuffer().setContents(
				"package test0538;\n" +
				"public class A {\n" +
				"  int i;\n" +
				"}"
			);
			CompilationUnit unit = sourceUnit.reconcile(true, false, null, null);
			assertNotNull("No compilation unit", unit);
		} finally {
			sourceUnit.discardWorkingCopy();
		}
	}
	/*
	 * Ensures that no AST is created during reconcile if not requested.
	 */
	public void test0538b() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0538", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		try {
			sourceUnit.becomeWorkingCopy(null, null);
			sourceUnit.getBuffer().setContents(
				"package test0538;\n" +
				"public class A {\n" +
				"  int i;\n" +
				"}"
			);
			CompilationUnit unit = sourceUnit.reconcile(false, false, null, null);
			assertNull("Unexpected compilation unit", unit);
		} finally {
			sourceUnit.discardWorkingCopy();
		}
	}
	/*
	 * Ensures that no AST is created during reconcile if consistent.
	 */
	public void test0538c() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0538", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		try {
			sourceUnit.becomeWorkingCopy(null, null);
			CompilationUnit unit = sourceUnit.reconcile(true, false, null, null);
			assertNull("Unexpected compilation unit", unit);
		} finally {
			sourceUnit.discardWorkingCopy();
		}
	}
	/*
	 * Ensures that bindings are created during reconcile if the problem requestor is active.
	 */
	public void test0538d() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0538", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		try {
			ReconcilerTests.ProblemRequestor pbRequestor = new ReconcilerTests.ProblemRequestor();
			sourceUnit.becomeWorkingCopy(pbRequestor, null);
			sourceUnit.getBuffer().setContents(
				"package test0538;\n" +
				"public class A {\n" +
				"  Object field;\n" +
				"}"
			);
			CompilationUnit unit = sourceUnit.reconcile(true, false, null, null);
			ASTNode node = getASTNode(unit, 0, 0);
			assertNotNull("No node", node);
			assertTrue("not a field declaration", node.getNodeType() == ASTNode.FIELD_DECLARATION);
			FieldDeclaration declaration = (FieldDeclaration) node;
			Type type = declaration.getType();
			ITypeBinding typeBinding = type.resolveBinding();
			assertNotNull("No type binding", typeBinding); 
			assertEquals("Wrong name", "Object", typeBinding.getName());
		} finally {
			sourceUnit.discardWorkingCopy();
		}
	}
	/*
	 * Ensures that bindings are created during reconcile if force problem detection is turned on.
	 */
	public void test0538e() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0538", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		try {
			ReconcilerTests.ProblemRequestor pbRequestor = new ReconcilerTests.ProblemRequestor();
			sourceUnit.becomeWorkingCopy(pbRequestor, null);
			CompilationUnit unit = sourceUnit.reconcile(true, true/*force pb detection*/, null, null);
			ASTNode node = getASTNode(unit, 0);
			assertNotNull("No node", node);
			assertTrue("not a type declaration", node.getNodeType() == ASTNode.TYPE_DECLARATION);
			TypeDeclaration declaration = (TypeDeclaration) node;
			ITypeBinding typeBinding = declaration.resolveBinding();
			assertNotNull("No type binding", typeBinding); 
			assertEquals("Wrong name", "A", typeBinding.getName());
		} finally {
			sourceUnit.discardWorkingCopy();
		}
	}
	/*
	 * Ensures that bindings are created during reconcile if force problem detection is turned on.
	 * Case of a unit containing an anonymous type.
	 * (regression test for bug 55102 NPE when using ICU.reconcile(GET_AST_TRUE, ...))
	 */
	public void test0538f() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0538", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		try {
			ReconcilerTests.ProblemRequestor pbRequestor = new ReconcilerTests.ProblemRequestor();
			sourceUnit.becomeWorkingCopy(pbRequestor, null);
			sourceUnit.getBuffer().setContents(
				"package test0538;\n" +
				"public class A {\n" +
				"  void foo() {\n" +
				"    new Object() {\n" +
				"      void bar() {\n" +
				"      }\n" +
				"    };\n" +
				"  }\n" +
				"}"
			);
			CompilationUnit unit = sourceUnit.reconcile(true, true/*force pb detection*/, null, null);
			ASTNode node = getASTNode(unit, 0);
			assertNotNull("No node", node);
		} finally {
			sourceUnit.discardWorkingCopy();
		}
	}
	/*
	 * Ensures that bindings are created during reconcile if force problem detection is turned on.
	 * Case of a unit containing an anonymous type.
	 * (regression test for bug 55102 NPE when using ICU.reconcile(GET_AST_TRUE, ...))
	 */
	public void test0538g() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0538", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		try {
			ReconcilerTests.ProblemRequestor pbRequestor = new ReconcilerTests.ProblemRequestor();
			sourceUnit.becomeWorkingCopy(pbRequestor, null);
			sourceUnit.getBuffer().setContents(
				"package test0538;\n" +
				"public class A {\n" +
				"  void foo() {\n" +
				"    new Object() {\n" +
				"      void bar() {\n" +
				"      }\n" +
				"    };\n" +
				"  }\n" +
				"}"
			);
			sourceUnit.reconcile(false/*don't create AST*/, false/* don't force pb detection*/, null, null);
			CompilationUnit unit = sourceUnit.reconcile(true/*create AST*/, true/*force pb detection*/, null, null);
			ASTNode node = getASTNode(unit, 0);
			assertNotNull("No node", node);
		} finally {
			sourceUnit.discardWorkingCopy();
		}
	}
	/**
	 * http://dev.eclipse.org/bugs/show_bug.cgi?id=53477
	 */
	public void test0539() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0539", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, false);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0, 1, 0);
		assertNotNull("No node", node);
		assertTrue("not an expression statement", node.getNodeType() == ASTNode.EXPRESSION_STATEMENT);
		ExpressionStatement expressionStatement = (ExpressionStatement) node;
		Expression expression = expressionStatement.getExpression();
		assertTrue("not a class instance creation", expression.getNodeType() == ASTNode.CLASS_INSTANCE_CREATION);
		ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) expression;
		checkSourceRange(classInstanceCreation, "new A(){}.new Inner(){/*x*/}", source);
		AnonymousClassDeclaration anonymousClassDeclaration = classInstanceCreation.getAnonymousClassDeclaration();
		Expression expression2 = classInstanceCreation.getExpression();
		assertTrue("not a class instance creation", expression2.getNodeType() == ASTNode.CLASS_INSTANCE_CREATION);
		ClassInstanceCreation classInstanceCreation2 = (ClassInstanceCreation) expression2;
		AnonymousClassDeclaration anonymousClassDeclaration2 = classInstanceCreation2.getAnonymousClassDeclaration();
		assertNotNull("No anonymous class declaration", anonymousClassDeclaration2);
		checkSourceRange(anonymousClassDeclaration2, "{}", source);
		assertNotNull("No anonymous class declaration", anonymousClassDeclaration);
		checkSourceRange(anonymousClassDeclaration, "{/*x*/}", source);
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=54431
	 */
	public void test0540() {
		char[] source = 
				("package test0540;\n" +  //$NON-NLS-1$
				"\n" +  //$NON-NLS-1$
				"class Test {\n" +  //$NON-NLS-1$
				"	public void foo(int arg) {\n" +//$NON-NLS-1$
				"		assert true;\n" +//$NON-NLS-1$
				"	}\n" +  //$NON-NLS-1$
				"}").toCharArray(); //$NON-NLS-1$
		IJavaProject project = getJavaProject("Converter"); //$NON-NLS-1$
		Map options = project.getOptions(true);
		options.put(JavaCore.COMPILER_SOURCE, JavaCore.VERSION_1_4);
		options.put(JavaCore.COMPILER_CODEGEN_TARGET_PLATFORM, JavaCore.VERSION_1_4);
		options.put(JavaCore.COMPILER_COMPLIANCE, JavaCore.VERSION_1_4);
		ASTNode result = runConversion(source, "Test.java", project, options); //$NON-NLS-1$
		assertNotNull("No compilation unit", result); //$NON-NLS-1$
		assertTrue("result is not a compilation unit", result instanceof CompilationUnit); //$NON-NLS-1$
		CompilationUnit compilationUnit = (CompilationUnit) result;
		assertEquals("Problems found", 0, compilationUnit.getProblems().length);
		ASTNode node = getASTNode(compilationUnit, 0);
		assertTrue("not a TypeDeclaration", node instanceof TypeDeclaration); //$NON-NLS-1$
		TypeDeclaration typeDeclaration = (TypeDeclaration) node;
		ITypeBinding typeBinding = typeDeclaration.resolveBinding();
		assertNotNull("No type binding", typeBinding); //$NON-NLS-1$
		assertEquals("Wrong name", "Test", typeBinding.getName()); //$NON-NLS-1$ //$NON-NLS-2$
		assertEquals("Wrong package", "test0540", typeBinding.getPackage().getName()); //$NON-NLS-1$ //$NON-NLS-2$
		assertTrue("Not an interface", typeBinding.isClass()); //$NON-NLS-1$
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=56697
	 */
	public void test0541() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0541", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ASTNode result = runConversion(sourceUnit, true);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0, 0);
		assertEquals("not a field declaration", ASTNode.FIELD_DECLARATION, node.getNodeType());
		class Change14FieldAccessASTVisitor extends ASTVisitor {
			int counter;
			Change14FieldAccessASTVisitor() {
				counter = 0;
			}
			public void endVisit(QualifiedName qualifiedName) {
				IBinding i_binding = qualifiedName.getQualifier().resolveBinding();						
				ITypeBinding type_binding = qualifiedName.getQualifier().resolveTypeBinding();
				if (i_binding == null || type_binding == null) {
					counter++;
				}
			}
		}
		Change14FieldAccessASTVisitor visitor = new Change14FieldAccessASTVisitor();
		unit.accept(visitor);
		assertEquals("Missing binding", 0, visitor.counter);
	}
	
	/**
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=54431
	 * TODO (olivier) investigate why test is currently failing
	 */
	public void _test0542() throws JavaModelException {
		ICompilationUnit sourceUnit = getCompilationUnit("Converter", "src", "test0542", "A.java"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		char[] source = sourceUnit.getSource().toCharArray();
		ASTNode result = runConversion(sourceUnit, true);
		final CompilationUnit unit = (CompilationUnit) result;
		assertEquals("Wrong number of problems", 0, unit.getProblems().length); //$NON-NLS-1$
		ASTNode node = getASTNode(unit, 0, 0);
		assertTrue("not a field declaration", node instanceof FieldDeclaration); //$NON-NLS-1$
		FieldDeclaration fieldDeclaration = (FieldDeclaration) node;
		List fragments = fieldDeclaration.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(0);
		IVariableBinding variableBinding = fragment.resolveBinding();
		assertNotNull("No binding", variableBinding);
		assertEquals("Wrong name", "STRING_FIELD", variableBinding.getName());
		Object constantValue = variableBinding.getConstantValue();
		assertNotNull("No constant", constantValue);
		assertEquals("Wrong value", "Hello world!", constantValue);
		Expression initializer = fragment.getInitializer();
		assertNotNull("No initializer", initializer);
		checkSourceRange(initializer, "\"Hello world!\"", source);

		node = getASTNode(unit, 0, 1);
		assertTrue("not a field declaration", node instanceof FieldDeclaration); //$NON-NLS-1$
		fieldDeclaration = (FieldDeclaration) node;
		fragments = fieldDeclaration.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		fragment = (VariableDeclarationFragment) fragments.get(0);
		variableBinding = fragment.resolveBinding();
		assertNotNull("No binding", variableBinding);
		assertEquals("Wrong name", "BOOLEAN_FIELD", variableBinding.getName());
		constantValue = variableBinding.getConstantValue();
		assertNotNull("No constant", constantValue);
		assertEquals("Wrong value", new Boolean(true), constantValue);
		initializer = fragment.getInitializer();
		assertNotNull("No initializer", initializer);
		checkSourceRange(initializer, "true", source);

		node = getASTNode(unit, 0, 2);
		assertTrue("not a field declaration", node instanceof FieldDeclaration); //$NON-NLS-1$
		fieldDeclaration = (FieldDeclaration) node;
		fragments = fieldDeclaration.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		fragment = (VariableDeclarationFragment) fragments.get(0);
		variableBinding = fragment.resolveBinding();
		assertNotNull("No binding", variableBinding);
		assertEquals("Wrong name", "BYTE_FIELD", variableBinding.getName());
		constantValue = variableBinding.getConstantValue();
		assertNotNull("No constant", constantValue);
		assertEquals("Wrong value", new Byte((byte)1), constantValue);
		initializer = fragment.getInitializer();
		assertNotNull("No initializer", initializer);
		checkSourceRange(initializer, "1", source);

		node = getASTNode(unit, 0, 3);
		assertTrue("not a field declaration", node instanceof FieldDeclaration); //$NON-NLS-1$
		fieldDeclaration = (FieldDeclaration) node;
		fragments = fieldDeclaration.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		fragment = (VariableDeclarationFragment) fragments.get(0);
		variableBinding = fragment.resolveBinding();
		assertNotNull("No binding", variableBinding);
		assertEquals("Wrong name", "CHAR_FIELD", variableBinding.getName());
		constantValue = variableBinding.getConstantValue();
		assertNotNull("No constant", constantValue);
		assertEquals("Wrong value", new Character('{'), constantValue);
		initializer = fragment.getInitializer();
		assertNotNull("No initializer", initializer);
		checkSourceRange(initializer, "\'{\'", source);

		node = getASTNode(unit, 0, 4);
		assertTrue("not a field declaration", node instanceof FieldDeclaration); //$NON-NLS-1$
		fieldDeclaration = (FieldDeclaration) node;
		fragments = fieldDeclaration.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		fragment = (VariableDeclarationFragment) fragments.get(0);
		variableBinding = fragment.resolveBinding();
		assertNotNull("No binding", variableBinding);
		assertEquals("Wrong name", "DOUBLE_FIELD", variableBinding.getName());
		constantValue = variableBinding.getConstantValue();
		assertNotNull("No constant", constantValue);
		assertEquals("Wrong value", new Double("3.1415"), constantValue);
		initializer = fragment.getInitializer();
		assertNotNull("No initializer", initializer);
		checkSourceRange(initializer, "3.1415", source);
		
		node = getASTNode(unit, 0, 5);
		assertTrue("not a field declaration", node instanceof FieldDeclaration); //$NON-NLS-1$
		fieldDeclaration = (FieldDeclaration) node;
		fragments = fieldDeclaration.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		fragment = (VariableDeclarationFragment) fragments.get(0);
		variableBinding = fragment.resolveBinding();
		assertNotNull("No binding", variableBinding);
		assertEquals("Wrong name", "FLOAT_FIELD", variableBinding.getName());
		constantValue = variableBinding.getConstantValue();
		assertNotNull("No constant", constantValue);
		assertEquals("Wrong value", new Float("3.14159f"), constantValue);
		initializer = fragment.getInitializer();
		assertNotNull("No initializer", initializer);
		checkSourceRange(initializer, "3.14159f", source);

		node = getASTNode(unit, 0, 6);
		assertTrue("not a field declaration", node instanceof FieldDeclaration); //$NON-NLS-1$
		fieldDeclaration = (FieldDeclaration) node;
		fragments = fieldDeclaration.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		fragment = (VariableDeclarationFragment) fragments.get(0);
		variableBinding = fragment.resolveBinding();
		assertNotNull("No binding", variableBinding);
		assertEquals("Wrong name", "INT_FIELD", variableBinding.getName());
		constantValue = variableBinding.getConstantValue();
		assertNotNull("No constant", constantValue);
		assertEquals("Wrong value", Integer.valueOf("7fffffff", 16), constantValue);
		initializer = fragment.getInitializer();
		assertNotNull("No initializer", initializer);
		checkSourceRange(initializer, "Integer.MAX_VALUE", source);
		
		node = getASTNode(unit, 0, 7);
		assertTrue("not a field declaration", node instanceof FieldDeclaration); //$NON-NLS-1$
		fieldDeclaration = (FieldDeclaration) node;
		fragments = fieldDeclaration.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		fragment = (VariableDeclarationFragment) fragments.get(0);
		variableBinding = fragment.resolveBinding();
		assertNotNull("No binding", variableBinding);
		assertEquals("Wrong name", "LONG_FIELD", variableBinding.getName());
		constantValue = variableBinding.getConstantValue();
		assertNotNull("No constant", constantValue);
		assertEquals("Wrong value", new Long("34"), constantValue);
		initializer = fragment.getInitializer();
		assertNotNull("No initializer", initializer);
		checkSourceRange(initializer, "34L", source);
		
		node = getASTNode(unit, 0, 8);
		assertTrue("not a field declaration", node instanceof FieldDeclaration); //$NON-NLS-1$
		fieldDeclaration = (FieldDeclaration) node;
		fragments = fieldDeclaration.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		fragment = (VariableDeclarationFragment) fragments.get(0);
		variableBinding = fragment.resolveBinding();
		assertNotNull("No binding", variableBinding);
		assertEquals("Wrong name", "SHORT_FIELD", variableBinding.getName());
		constantValue = variableBinding.getConstantValue();
		assertNotNull("No constant", constantValue);
		assertEquals("Wrong value", new Short("130"), constantValue);
		initializer = fragment.getInitializer();
		assertNotNull("No initializer", initializer);
		checkSourceRange(initializer, "130", source);
		
		node = getASTNode(unit, 0, 9);
		assertTrue("not a field declaration", node instanceof FieldDeclaration); //$NON-NLS-1$
		fieldDeclaration = (FieldDeclaration) node;
		fragments = fieldDeclaration.fragments();
		assertEquals("Wrong size", 1, fragments.size());
		fragment = (VariableDeclarationFragment) fragments.get(0);
		variableBinding = fragment.resolveBinding();
		assertNotNull("No binding", variableBinding);
		assertEquals("Wrong name", "int_field", variableBinding.getName());
		constantValue = variableBinding.getConstantValue();
		assertNull("Got a constant", constantValue);
		initializer = fragment.getInitializer();
		assertNotNull("No initializer", initializer);
		checkSourceRange(initializer, "Integer.MAX_VALUE", source);
	}
}