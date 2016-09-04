//
//  ABT.swift
//  Valence
//
//  Created by Robert Widmann on 4/2/16.
//  Copyright © 2016 CodaFi. All rights reserved.
//

public protocol ABT {
	associatedtype ABTVariable
	associatedtype ABTSymbol
	associatedtype ABTMetaVariable
	associatedtype ABTOperator
}

/// An `AbstractBindingTree` is a tree representation of multi-sorted 
/// binding-aware abstract syntactic structure with annotations at all levels.
///
/// When developing programming languages, it is important to keep track of 
/// binding at all levels to correctly implement variable binding,
/// quantification, scope, etc.  An Abstract Binding Tree handles the nitty
/// gritty of keeping track of binding and scope through a system of arities 
/// à la Martin Löf ~( http://www.cse.chalmers.se/research/group/logic/book/book.pdf ).
/// This particular implementation also handles the checking of symbol sorts
/// automatically and hence allows for multiple kinds of syntax to coexist in
/// the binding structure.
///
/// When reasoning about binding structure it is often helpful to break out of
/// an ABT and take a look at only the relevant slices of data contained at each
/// level of structure.  For that, operations are provided to splice into and
/// out of `View`s of the ABT.
///
/// For an in-depth look at structural, categorical, and topological semantics
/// of abstract binding trees, see ~( https://arxiv.org/pdf/1601.06298.pdf ).
/// This implementation was informed by the very same one that inspired the
/// forementioned paper ~( https://github.com/RedPRL/sml-typed-abts ) by 
/// Jon Sterling.
///
///
/// An `AbstractBindingTree` is parametrized by 5 type variables:
///
/// - parameters
///     - V: The type of variables.
///     - S: The type of symbols - they parametrize operators and can be used to
///          implement open sums and assignable references.
///     - M: The type of metavariables - they stand in for an abstraction.
///     - O: The type of operators - they define the principle operations 
///          represented by the nodes of the tree and come with an array of 
///          bound arguments and access to symbols.
///     - A: The type of annotations at each level of the syntax tree - e.g.
///          source locations.
public struct AbstractBindingTree<V : Symbol, S : Symbol, M : Symbol, O : Operator, A> : ABT, CustomStringConvertible, Equatable
	where O.I == LocallyNameless<S>
{
	private let abt : AbstractBindingTreeImpl<V, S, M, O, A>
	private let an : A?
	
	public typealias ABTVariable = V
	public typealias ABTSymbol = S
	public typealias ABTMetaVariable = M
	public typealias ABTOperator = O
	
	public typealias ABTView = View<V, S, M, O, A>
	public typealias ABTBView = BindingView<V, S, M, O, A>
	
	public typealias MetaContext = Dictionary<M, Valence<O.S>> 
	public typealias VariableContext = Dictionary<V, O.S>
	public typealias SymbolContext = Dictionary<S, O.S>
	public typealias Context = (MetaContext, SymbolContext, VariableContext)

	public typealias MetaEnv = Dictionary<M, BindingView<V, S, M, O, A>>
	public typealias VarEnv = Dictionary<V, AbstractBindingTree<V, S, M, O, A>>
	public typealias SymEnv = Dictionary<S, S>

	/// Creates an Abstract Binding Tree from a `View` of an ABT and, for 
	/// bindings, checks the sort of the variables in the arguments.
	///
	/// This is the `into` operation from CMU's ABT homework.
	public init(checking view : View<V, S, M, O, A>, _ canSort : O.S) {
		switch view {
		case let .Variable(x):
			self.init(.Variable(.Free(x), canSort), nil)
		case let .Apply(op, args):
			precondition(canSort == op.arity.sort)
			self.init(makeApp(op, zip(args, op.arity.valences).map { (ls, rs) in
				return Abstraction(checking: ls, rs)
			}), nil)
		case let .MetaApply(x, syms, ms):
			let vsorts = ms.map { abt in abt.sort }
			let freedSyms : [(LocallyNameless<S>, O.S)] = syms.map { (u, sort) in (.Free(u), sort) }
			self.init(makeMetaApp(x, canSort, freedSyms, zip(ms, vsorts).map { (m, sort) in
				precondition(sort == m.sort)
				return m
			}), nil)
		}
	}

	/// A convenience for constructing operators applied to binding views.
	public init(applying o : O, to es : [BindingView<V, S, M, O, A>]) {
		self.init(checking: .Apply(o, es), o.arity.sort)
	}

	fileprivate init(_ abt : AbstractBindingTreeImpl<V, S, M, O, A>, _ an : A?) {
		self.abt = abt
		self.an = an
	}

	/// Pattern match on the view of an ABT and its sort.
	public var infer : (View<V, S, M, O, A>, O.S) {
		switch self.abt {
		case let .Variable(v, sort):
			return (.Variable(v.getFree), sort)
		case let .Apply(op, args):
			return (.Apply(op, args.ann.map { $0.infer.0 }), op.arity.sort)
		case let .MetaApply((mv, term), syms, Ms):
			let freedSyms : [(S, O.S)] = syms.map { (v, sort) in (v.getFree, sort) }
			return (.MetaApply(mv, freedSyms, Ms.ann), term)
		}
	}

	/// Pattern match on the view of an ABT.  
	///
	/// This is the `out` operation from CMU's ABT homework.
	public var toView : View<V, S, M, O, A> {
		return self.infer.0
	}

	/// Pattern match on teh sort of an ABT.
	public var sort : O.S {
		return self.infer.1
	}

	/// All free metavariable bindings in scope at this level of ABT structure.
	///
	/// A metavariable context consists of bindings of valences to 
	/// symbol-indexed metavariables.
	public var metavariableContext : MetaContext {
		switch self.abt {
		case .Variable(_, _):
			return [:]
		case let .Apply(_, ann):
			return ann.ctx.0
		case let .MetaApply((mv, term), syms, ann):
			let ms = ann.ann
			let (mctx, _, _) = ann.ctx
			let vl = Valence(syms.map { $1 }, ms.map { $0.sort }, term)
			return mctx.insert(mv, v: vl)
		}
	}

	/// All free symbol bindings in scope at this level of ABT structure.
	///
	/// A symbol context consists of bindings of symbols to operator sorts.
	public var symbolContext : SymbolContext {
		switch self.abt {
		case .Variable(_, _):
			return [:]
		case let .Apply(op, ann):
			return op.support.reduce(ann.ctx.1) { (ctx, v) in
				guard case let .Free(u) = v.0 else {
					return ctx
				}
				return ctx.insert(u, v: v.1)
			}
		case let .MetaApply(_, syms, ann):
			return syms.reduce(ann.ctx.1) { (ctx, v) in
				guard case let (.Free(u), sort) = v else {
					return ctx
				}
				return ctx.insert(u, v: sort)
			}
		}
	}

	/// All free variable bindings in scope at this level of ABT structure.
	///
	/// A variable context consists of bindings of symbols to their associated
	/// sorts.
	public var variableContext : VariableContext {
		switch self.abt {
		case let .Variable(.Free(x), sort):
			return [x: sort]
		case .Variable(_, _):
			return [:]
		case let .Apply(_, ann):
			return ann.ctx.2
		case let .MetaApply((_, _), _, ann):
			return ann.ctx.2
		}
	}

	/// Calculates all free metavariables, free variables and free symbols.
	public var context : Context {
		return (self.metavariableContext, self.symbolContext, self.variableContext)
	}

	/// Simultaneous substitution of a bound variable binding by an ABT.
	public func substitute(_ n : AbstractBindingTree<V, S, M, O, A>, for x : V) -> AbstractBindingTree<V, S, M, O, A> {
		return self.substEnv([x: n])
	}

	/// Simultaneous substition of symbols.
	public func rename(_ s : S, for t : S) -> AbstractBindingTree<V, S, M, O, A> {
		return self.renameEnv([s: t])
	}

	/// Simultaneous substitution of a set of variable-to-ABT bindings.
	public func substEnv(_ subs : VarEnv) -> AbstractBindingTree<V, S, M, O, A> {
		return self.map { m in
			switch m {
			case let .Variable(.Free(x), _):
				return subs[x]?.abt ?? m
			case .Variable(_, _):
				return m
			case let .Apply(op, args):
				return makeApp(op, args.ann.map { e in
					return e.map { e2 in
						return e2.substEnv(subs)
					}
				})
			case let .MetaApply(m, syms, ms):
				return makeMetaApp(m.0, m.1, syms, ms.ann.map { m2 in m2.substEnv(subs) })
			}
		}
	}

	/// Simultaneous substitution of symbols.
	public func renameEnv(_ subs : SymEnv) -> AbstractBindingTree<V, S, M, O, A> {
		return self.map { m in
			switch m {
			case .Variable(_, _):
				return m
			case let .Apply(op, args):
				return makeApp(op, args.ann.map { e in
					return e.map { e2 in
						return e2.renameEnv(subs)
					}
				})
			case let .MetaApply((mv, term), syms, ms):
				return makeMetaApp(mv, term, syms.map { (l, s) in
					return (l.map { (u : S) -> S in
						return subs[u] ?? u
					}, s)
				}, ms.ann.map { m in
					return m.renameEnv(subs)
				})
			}
		}
	}

	/// Simultaneous substition of a metavariable by an abstraction.
	public func metasubstEnv(_ subs : MetaEnv) -> AbstractBindingTree<V, S, M, O, A> {
		let (view, sort) = self.infer
		switch view {
		case .Variable(_):
			return self
		case let .Apply(op, args):
			return AbstractBindingTree<V, S, M, O, A>(checking: .Apply(op, args.map { e in
				return e.map { e2 in
					return e2.metasubstEnv(subs)
				}
			}), sort)
		case let .MetaApply(mv, syms, ms):
			let m2 = ms.map { m in
				return m.metasubstEnv(subs)
			}
			if case let .some(.Binding(vs, xs, m)) = subs[mv] {
				let renaming : SymEnv = zip(vs, syms).reduce(SymEnv()) { (r, t) in
					let (v, (u, _)) = t
					return r.insert(v, v: u)
				}
				let subs : VarEnv = zip(xs, m2).reduce(VarEnv()) { (subs, t) in
					let (x, m) = t
					return subs.insert(x, v: m)
				}
				return m.renameEnv(renaming).substEnv(subs)
			}
			return AbstractBindingTree<V, S, M, O, A>(checking: .MetaApply(mv, syms, m2), sort)
		}
	}

	public var description : String {
		switch self.infer.0 {
		case let .Variable(x):
			return "\(x)"
		case let .Apply(ops, es):
			return ops.description + " " + es.map({ $0.description }).joined(separator: "; ")
		case let .MetaApply(mv, us, ms):
			let us2 = us.map({ $0.0.description }).joined(separator: ",")
			let ms2 = ms.map({ $0.description }).joined(separator: ",")
			return "#\(mv) {\(us2)} [\(ms2)]"
		}
	}

	public static func ==(l : AbstractBindingTree<V, S, M, O, A>, r : AbstractBindingTree<V, S, M, O, A>) -> Bool {
		switch (l.abt, r.abt) {
		case let (.Variable(v1, _), .Variable(v2, _)):
			return v1 == v2
		case let (.Apply(opl, esl), .Apply(opr, esr)):
			return opl == opr
				&& esl.ann == esr.ann
		case let (.MetaApply((mv, _), us, ms), .MetaApply((mv2, _), us2, ms2)):
			return mv == mv2
				&& zip(us, us2).reduce(true) { (r, x) in r && x.0.0 == x.1.0 && x.0.1 == x.1.1  }
				&& ms.ann == ms2.ann
		default:
			return false
		}
	}


	fileprivate func map(_ f : (AbstractBindingTreeImpl<V, S, M, O, A>) -> AbstractBindingTreeImpl<V, S, M, O, A>) -> AbstractBindingTree<V, S, M, O, A> {
		return AbstractBindingTree(f(self.abt), self.an)
	}

	fileprivate func liberateSymbolAnn(_ u : (S, O.S), _ coord : Coordinate) -> AbstractBindingTree<V, S, M, O, A> {
		return self.map { m in
			return m.liberateSymbol(u, coord)
		}
	}

	fileprivate func liberateSymbols(_ u : [(S, O.S)]) -> AbstractBindingTree<V, S, M, O, A> {
		return foldStar(u, f: { v, c, M in
			return M.liberateSymbolAnn(v, c)
		}, t: self)
	}


	fileprivate func imprisonSymbolAnn(_ v : S, _ sort : O.S, _ coord : Coordinate) -> AbstractBindingTree<V, S, M, O, A> {
		return self.map { m in
			return m.imprisonSymbol(v, sort, coord)
		}
	}

	fileprivate func imprisonSymbols(_ u : [(S, O.S)]) -> AbstractBindingTree<V, S, M, O, A> {
		return foldStar(u, f: { v, c, M in
			return M.imprisonSymbolAnn(v.0, v.1, c)
		}, t: self)
	}


	fileprivate func liberateVariableAnn(_ v : (V, O.S), _ coord : Coordinate) -> AbstractBindingTree<V, S, M, O, A> {
		return self.map { m in
			return m.liberateVariable(v, coord)
		}
	}

	fileprivate func liberateVariables(_ u : [(V, O.S)]) -> AbstractBindingTree<V, S, M, O, A> {
		return foldStar(u, f: { v, c, M in
			return M.liberateVariableAnn(v, c)
		}, t: self)
	}

	fileprivate func imprisonVariableAnn(_ v : V, _ sort : O.S, _ coord : Coordinate) -> AbstractBindingTree<V, S, M, O, A> {
		return self.map { m in
			return m.imprisonVariable(v, sort, coord)
		}
	}

	fileprivate func imprisonVariables(_ u : [(V, O.S)]) -> AbstractBindingTree<V, S, M, O, A> {
		return foldStar(u, f: { v, c, M in
			return M.imprisonVariableAnn(v.0, v.1, c)
		}, t: self)
	}
	
}

// MARK: Implementation Details Follow

internal struct ContextAnnotatedTree<V : Symbol, S : Symbol, M : Symbol, O : Operator, A>
	where O.I == LocallyNameless<S>
{
	let ann : A
	let ctx : AbstractBindingTree<V, S, M, O, A>.Context

	init(_ x : A, _ ctx : AbstractBindingTree<V, S, M, O, A>.Context) {
		self.ann = x
		self.ctx = ctx
	}
}

indirect enum AbstractBindingTreeImpl<V : Symbol, S : Symbol, M : Symbol, O : Operator, A>
	where O.I == LocallyNameless<S>
{
	public typealias Annotation = A

	case Variable(LocallyNameless<V>, O.S)
	case Apply(O, ContextAnnotatedTree<V, S, M, O, [Abstraction<V, S, M, O, A>]>)
	case MetaApply((M, O.S), [(LocallyNameless<S>, O.S)], ContextAnnotatedTree<V, S, M, O, [AbstractBindingTree<V, S, M, O, A>]>)

	func imprisonSymbol(_ v : S, _ sort : O.S, _ coord : Coordinate) -> AbstractBindingTreeImpl<V, S, M, O, A> {
		switch self {
		case .Variable(_, _):
			return self
		case let .Apply(ops, es):
			let ctx2 = (es.ctx.0, es.ctx.1.remove(v), es.ctx.2)
			return .Apply(ops, ContextAnnotatedTree(es.ann.map { e in
				return e.traverse({ (c, m) in
					return m.imprisonSymbolAnn(v, sort, coord)
				}, c: coord)
			}, ctx2))
		case let .MetaApply(m, us, Ms):
			return .MetaApply(m, us.map { (l, s) in 
				return (l.flatMap { v2 in
					if v == v2 {
						return .Bound(coord)
					}
					return .Free(v2)
				}, s)
			}, ContextAnnotatedTree(Ms.ann.map { m in m.imprisonSymbolAnn(v, sort, coord) }, Ms.ctx)) // FIXME
		}
	}
	
	func imprisonVariable(_ v : V, _ sort : O.S, _ coord : Coordinate) -> AbstractBindingTreeImpl<V, S, M, O, A> {
		switch self {
		case let .Variable(.Free(v2), sig):
			if v == v2 {
				precondition(sig == sort)
				return .Variable(.Bound(coord), sig)
			}
			return self
		case .Variable(_, _):
			return self
		case let .Apply(ops, es):
			return makeApp(ops, es.ann.map { e in
				return e.traverse({ (c, m) in
					return m.imprisonVariableAnn(v, sort, c)
				}, c: coord)
			})
		case let .MetaApply((mv, sort), us, ms):
			return makeMetaApp(mv, sort, us, ms.ann.map { $0.imprisonVariableAnn(v, sort, coord) })
		}
	}
	
	func liberateSymbol(_ us : (S, O.S), _ coord : Coordinate) -> AbstractBindingTreeImpl<V, S, M, O, A> {
		let (u, sort) = us
		switch self {
		case .Variable(_, _):
			return self
		case let .Apply(ops, es):
			return makeApp(ops, es.ann.map { e in
				return e.traverse({ (c, m) in
					return m.liberateSymbolAnn((u, sort), c)
				}, c: coord)
			})
		case let .MetaApply((x, sort), us, ms):
			return makeMetaApp(x, sort, us.map { (l, s) in
				if case let .Bound(coord2) = l {
					if coord == coord2 {
						return (.Free(u), s)
					}
					return (.Bound(coord2), s)
				}
				return (l, s)
			}, ms.ann.map { m in
				return m.liberateSymbolAnn((u, sort), coord)
			})
		}
	}

	func liberateVariable(_ vs : (V, O.S), _ coord : Coordinate) -> AbstractBindingTreeImpl<V, S, M, O, A> {
		let (v, sort) = vs
		switch self {
		case .Variable(.Free(_), _):
			return self
		case let .Variable(.Bound(coord2), sort):
			if coord == coord2 {
				return .Variable(.Free(v), sort)
			}
			return self
		case let .Apply(ops, es):
			return makeApp(ops, es.ann.map { e in
				return e.traverse({ (c, m) in
					return m.liberateVariableAnn((v, sort), c)
				}, c: coord)
			})
		case let .MetaApply((x, sort), us, ms):
			return makeMetaApp(x, sort, us, ms.ann.map { m in m.liberateVariableAnn((v, sort), coord) })
		}
	}
}

internal struct Abstraction<V : Symbol, S : Symbol, M : Symbol, O : Operator, A>
	where O.I == LocallyNameless<S>
{
	let upsilon : [(String, O.S)]
	let gamma : [(String, O.S)]
	let m : AbstractBindingTree<V, S, M, O, A>

	init(_ upsilon : [(String, O.S)], _ gamma : [(String, O.S)], _ m : AbstractBindingTree<V, S, M, O, A>) {
		self.upsilon = upsilon
		self.gamma = gamma
		self.m = m
	}

	init(checking b : BindingView<V, S, M, O, A>, _ v : Valence<O.S>) {
		switch b {
		case let .Binding(us, xs, m):
			let (_, sort) = m.infer
			precondition(v.sort == sort)
			self.upsilon = Array(zip(us.map { $0.description }, v.symSorts))
			self.gamma = Array(zip(xs.map { $0.description }, v.varSorts))
			self.m = m.imprisonVariables(Array(zip(xs, v.varSorts))).imprisonSymbols(Array(zip(us, v.symSorts)))
		}
	}

	public var infer : (BindingView<V, S, M, O, A>, Valence<O.S>) {
		let syms = self.m.symbolContext
		let vars = self.m.variableContext
		let us = self.upsilon.map { (u, sort) in (S(fresh: syms, u), sort) }
		let xs = self.gamma.map { (x, sort) in (V(fresh: vars, x), sort) }
		let m2 = self.m.liberateVariables(xs).liberateSymbols(us)
		let (_, sort) = m.infer
		let valence = Valence(upsilon.map { $0.1 }, gamma.map { $0.1 }, sort)
		return (.Binding(us.map { x,_ in x }, xs.map { x,_ in x }, m2), valence)
	}

	public func map(_ f : (AbstractBindingTree<V, S, M, O, A>) -> AbstractBindingTree<V, S, M, O, A>) -> Abstraction<V, S, M, O, A> {
		return Abstraction(self.upsilon, self.gamma, f(self.m))
	}

	public func traverse(_ f : (Coordinate, AbstractBindingTree<V, S, M, O, A>) -> AbstractBindingTree<V, S, M, O, A>, c : Coordinate) -> Abstraction<V, S, M, O, A> {
		return self.map { v in
			return f(c.shiftRight, v)
		}
	}

	public var description : String {
		return (self.upsilon.isEmpty ? "" : "{\(self.upsilon)}")
			+ (self.upsilon.isEmpty ? "" : "[\(self.upsilon)]")
			+ ".\(m.description)"
	}
}

extension Abstraction : Equatable {
	public static func ==(l : Abstraction<V, S, M, O, A>, r : Abstraction<V, S, M, O, A>) -> Bool {
		return l.m == r.m
	}
}

private func makeApp<V : Symbol, S : Symbol, M : Symbol, O : Operator, A>(_ ops : O, _ es : [Abstraction<V, S, M, O, A>]) -> AbstractBindingTreeImpl<V, S, M, O, A>
{
	func merge(_ l : AbstractBindingTree<V, S, M, O, A>.Context, _ r : AbstractBindingTree<V, S, M, O, A>.Context) -> AbstractBindingTree<V, S, M, O, A>.Context
	{
		let (mctx1, sctx1, vctx1) = l
		let (mctx2, sctx2, vctx2) = r

		let mctx = mctx1.merge(mctx2)
		let sctx = sctx1.merge(sctx2)
		let vctx = vctx1.merge(vctx2)
		return (mctx, sctx, vctx)
	}

	return .Apply(ops, ContextAnnotatedTree(es, es.reduce(([:], [:], [:]) as AbstractBindingTree<V, S, M, O, A>.Context) { (ctx, a) -> AbstractBindingTree<V, S, M, O, A>.Context in
		let b : Abstraction<V, S, M, O, A> = a
		return merge(ctx, b.m.context)
	}))
}

private func makeMetaApp<V : Symbol, S : Symbol, M : Symbol, O : Operator, A>(_ mv : M, _ sort : O.S, _ us : [(LocallyNameless<S>, O.S)], _ ms : [AbstractBindingTree<V, S, M, O, A>]) -> AbstractBindingTreeImpl<V, S, M, O, A>
{
	func merge(_ l : AbstractBindingTree<V, S, M, O, A>.Context, _ r : AbstractBindingTree<V, S, M, O, A>.Context) -> AbstractBindingTree<V, S, M, O, A>.Context
	{
		let (mctx1, sctx1, vctx1) = l
		let (mctx2, sctx2, vctx2) = r

		let mctx = mctx1.merge(mctx2)
		let sctx = sctx1.merge(sctx2)
		let vctx = vctx1.merge(vctx2)
		return (mctx, sctx, vctx)
	}

	return AbstractBindingTreeImpl<V, S, M, O, A>.MetaApply((mv, sort), us, ContextAnnotatedTree(ms, ms.reduce(([:], [:], [:]) as AbstractBindingTree<V, S, M, O, A>.Context) { (ctx, a) -> AbstractBindingTree<V, S, M, O, A>.Context in
		return merge(ctx, a.context)
	}))
}

private func id<A>(_ : Coordinate, x : A) -> A {
	return x
}

private func foldStar<A, B>(_ xs : [A], f : @escaping (A, Coordinate, B) -> B, t : B) -> B {
	return foldMap(xs, f: { v in
		return { (c, M) in
			return f(v, c, M)
		}
	})(Coordinate(), t)
}

private func foldMap<A, B>(_ xs : [A], f : (A) -> ((Coordinate, B) -> B)) -> (Coordinate, B) -> B {
	return xs.reduce(id, { (phi, a) in
		let (f, g) = (f(a), phi)
		return { (coord, b) in f(coord, g(coord.shiftDown, b)) }
	})
}
