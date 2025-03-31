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
package org.eclipse.jdt.internal.javac;

import static com.sun.tools.javac.jvm.ByteCodes.athrow;

import com.sun.tools.javac.code.Kinds.Kind;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symbol.ClassSymbol;
import com.sun.tools.javac.code.Symbol.MethodSymbol;
import com.sun.tools.javac.code.Symbol.VarSymbol;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javac.code.Type.ErrorType;
import com.sun.tools.javac.comp.Attr;
import com.sun.tools.javac.comp.AttrContext;
import com.sun.tools.javac.comp.Env;
import com.sun.tools.javac.jvm.Gen;
import com.sun.tools.javac.tree.JCTree.JCArrayAccess;
import com.sun.tools.javac.tree.JCTree.JCAssign;
import com.sun.tools.javac.tree.JCTree.JCBinary;
import com.sun.tools.javac.tree.JCTree.JCClassDecl;
import com.sun.tools.javac.tree.JCTree.JCErroneous;
import com.sun.tools.javac.tree.JCTree.JCExpression;
import com.sun.tools.javac.tree.JCTree.JCExpressionStatement;
import com.sun.tools.javac.tree.JCTree.JCFieldAccess;
import com.sun.tools.javac.tree.JCTree.JCIdent;
import com.sun.tools.javac.tree.JCTree.JCIf;
import com.sun.tools.javac.tree.JCTree.JCInstanceOf;
import com.sun.tools.javac.tree.JCTree.JCLiteral;
import com.sun.tools.javac.tree.JCTree.JCMethodInvocation;
import com.sun.tools.javac.tree.JCTree.JCNewClass;
import com.sun.tools.javac.tree.JCTree.JCParens;
import com.sun.tools.javac.tree.JCTree.JCReturn;
import com.sun.tools.javac.tree.JCTree.JCThrow;
import com.sun.tools.javac.tree.JCTree.JCVariableDecl;
import com.sun.tools.javac.tree.TreeInfo;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.Names;

public class ProceedOnErrorGen extends Gen {

	public static void preRegister(Context context) {
		context.put(Gen.genKey, (Context.Factory<Gen>) ProceedOnErrorGen::new);
	}

	private Context context;

	protected ProceedOnErrorGen(Context context) {
		super(context);
		this.context = context;
	}

	private JCNewClass newRuntimeException() {
		var treeMaker = TreeMaker.instance(context);
		var res = treeMaker.NewClass(null, null, treeMaker.Ident(Names.instance(context).fromString(RuntimeException.class.getSimpleName())), List.of(treeMaker.Literal("Compile Error")), null);
		Attr.instance(context).attribStat(res, getAttrEnv());
		res.type = res.clazz.type; // ugly workaround
		return res;
	}

	@Override
	public boolean genClass(Env<AttrContext> env, JCClassDecl cdef) {
		try {
			return super.genClass(env, cdef) && verifyClassComplete(cdef.sym);
		} catch (Exception ex) {
			return false;
		}
	}

	private boolean verifyClassComplete(ClassSymbol sym) {
		return sym.type != null && !sym.type.isErroneous() &&
			!sym.getSuperclass().isErroneous() &&
			sym.getInterfaces().stream().noneMatch(Type::isErroneous) &&
			!sym.members().getSymbols(member ->
					(member instanceof VarSymbol field && field.type.isErroneous()) ||
					(member instanceof MethodSymbol method && (method.type.isErroneous() ||
						method.getReturnType().isErroneous() ||
						(method.params != null && method.params.stream().map(param -> param.type).anyMatch(Type::isErroneous)) ||
						method.getThrownTypes().stream().anyMatch(Type::isErroneous))
				)).iterator().hasNext();
	}

	@Override
	public void visitErroneous(JCErroneous that) {
		// workaround visitThrow assertions
		visitNewClass(newRuntimeException());
		getCode().emitop0(athrow);
	}

	@Override
	public void visitIdent(JCIdent tree) {
		if (tree.sym == null || tree.sym.kind == Kind.ERR || tree.sym.kind == Kind.STATICERR) {
			visitErroneous(null);
		} else {
			super.visitIdent(tree);
		}
	}

	@Override
	public void visitLiteral(JCLiteral tree) {
		if (tree.type == null || tree.type.isErroneous()) {
			visitErroneous(null);
		} else {
			super.visitLiteral(tree);
		}
	}
	
	@Override
	public void visitNewClass(JCNewClass tree) {
		if (tree.type == null || tree.type.isErroneous() || tree.args.stream().anyMatch(arg -> arg.type == null || arg.type.isErroneous())) {
			visitErroneous(null);
		} else {
			super.visitNewClass(tree);
		}
	}

	@Override
	public void visitApply(JCMethodInvocation tree) {
		if (tree.type.isErroneous() || tree.args.stream().anyMatch(arg -> arg.type == null || arg.type.isErroneous())) {
			visitErroneous(null);
		} else {
			Symbol meth1 = tree == null ? null : TreeInfo.symbol(tree.meth);
			Symbol base = meth1 == null ? null : meth1.baseSymbol();
			boolean isMethSym = base instanceof MethodSymbol;
			if( !isMethSym ) {
				return;
			}
			super.visitApply(tree);
		}
	}

	@Override
	public void visitSelect(JCFieldAccess tree) {
		if (tree.type.isErroneous()) {
			visitErroneous(null);
		} else {
			super.visitSelect(tree);
		}
	}

	@Override
	public void visitExec(JCExpressionStatement tree) {
		if (tree.expr == null || tree.expr instanceof JCErroneous || tree.expr.type.isErroneous()) {
			visitErroneous(null);
		} else {
			super.visitExec(tree);
		}
	}

	@Override
	public void visitAssign(JCAssign tree) {
		if (tree.lhs.type.isErroneous() || tree.rhs == null || tree.rhs.type == null || tree.rhs.type.isErroneous()) {
			visitErroneous(null);
		} else {
			super.visitAssign(tree);
		}
	}

	@Override
	public void visitIndexed(JCArrayAccess tree) {
		if (tree.type.isErroneous() || tree.getIndex() == null || tree.getIndex().type == null || tree.getIndex().type.isErroneous()) {
			visitErroneous(null);
		} else {
			super.visitIndexed(tree);
		}
	}

	@Override
	public void visitTypeTest(JCInstanceOf tree) {
		if (tree.getExpression() == null || tree.getExpression().type instanceof ErrorType) {
			visitErroneous(null);
		} else {
			super.visitTypeTest(tree);
		}
	}

	private boolean isValid(JCExpression exp) {
		return exp != null && !(exp instanceof JCErroneous)
				&& exp.type != null && !exp.type.isErroneous();
	}

	@Override
	public void visitThrow(JCThrow th) {
		if (!isValid(th.expr)) {
			visitErroneous(null);
		} else {
			super.visitThrow(th);
		}
	}

	@Override
	public void visitVarDef(JCVariableDecl varDef) {
		if (varDef.type == null || varDef.type.isErroneous()) {
			visitErroneous(null);
		} else {
			super.visitVarDef(varDef);
		}
	}

	@Override
	public void visitBinary(JCBinary binary) {
		if (!isValid(binary.lhs) || !isValid(binary.rhs)) {
			visitErroneous(null);
		} else {
			super.visitBinary(binary);
		}
	}

	@Override
	public void visitParens(JCParens parens) {
		if (!isValid(parens) || !isValid(parens.expr)) {
			visitErroneous(null);
		} else {
			super.visitParens(parens);
		}
	}

	@Override
	public void visitReturn(JCReturn tree) {
		if (tree == null || (tree.getExpression() != null && !isValid(tree.getExpression()))) {
			visitErroneous(null);
		} else {
			super.visitReturn(tree);
		}
	}

	@Override
	public void visitIf(JCIf tree) {
		if (tree == null || !isValid(tree.getCondition())) {
			visitErroneous(null);
		} else {
			super.visitIf(tree);
		}
	}
}
