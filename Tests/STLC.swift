//
//  STLC.swift
//  Valence
//
//  Created by Robert Widmann on 8/16/16.
//  Copyright © 2016 TypeLift. All rights reserved.
//

import Valence

fileprivate enum STLCSort : Sort {
	case TERM
	case TYPE

	var description : String {
		switch self {
		case .TERM:
			return "term"
		case .TYPE:
			return "type"
		}
	}

	static func ==(l : STLCSort, r : STLCSort) -> Bool {
		switch (l, r) {
		case (.TERM, .TERM):
			return true
		case (.TYPE, .TYPE):
			return true
		default:
			return false
		}
	}
}

fileprivate enum STLCOperator : Operator {
	typealias OperatorSort = STLCSort

	case VAR(Named)
	case LAM
	case AP

	case TY
	case ARR

	var arity : Arity<STLCSort> {
		switch self {
		case .VAR(_):
			return Arity([], .TERM)
		case .LAM: // λ
			return Arity([ // (x : τ) . e
				Valence([], [.TERM], .TERM),
			], .TERM)
		case .AP:
			return Arity([
				Valence([], [], .TERM), // e1
				Valence([], [], .TERM), // (e2)
			], .TERM)

		case .TY:
			return Arity([], .TYPE) // τ
		case .ARR:
			return Arity([
				Valence([], [], .TYPE), // τ1
										// ->
				Valence([], [], .TYPE), // τ2
			], .TYPE)
		}
	}

	var description : String {
		switch self {
		case let .VAR(i):
			return i.description
		case .LAM:
			return "lam"
		case .AP:
			return "app"
		case .TY:
			return "ty"
		case .ARR:
			return "arr"
		}
	}

	static func == (l : STLCOperator, r : STLCOperator) -> Bool {
		switch (l, r) {
		case let (.VAR(i), .VAR(j)):
			return i == j
		case (.LAM, .LAM):
			return true
		case (.AP, .AP):
			return true
		case (.TY, .TY):
			return true
		case (.ARR, .ARR):
			return true
		default:
			return false
		}
	}
}

fileprivate typealias STLCTerm = AbstractBindingTree<Named, Named, Named, STLCOperator, ()>
fileprivate typealias STLCτ = STLCTerm
fileprivate typealias STLCℯ = STLCTerm

fileprivate typealias Δ = Dictionary<Named, ()>
fileprivate typealias Γ = Dictionary<Named, STLCτ>

fileprivate enum ExprView<T, E> {
	case VAR(Named)
	case AP(E, E)
	case LAM(Named, E)
}

fileprivate enum TypeView {
	case TVAR(Named)
	case ARR(STLCτ, STLCτ)

	var intoSTLC : STLCτ {
		switch self {
		case let .TVAR(x):
			return STLCτ(checking: .Variable(x), .TYPE)
		case let .ARR(t1, t2):
			return STLCτ(applying: .ARR, to: [
				.Binding([], [], t1),
				.Binding([], [], t2),
			])
		}
	}
}

fileprivate func asExpression(_ e : STLCℯ) -> ExprView<STLCτ, STLCℯ> {
	switch e.infer {
	case let (.Variable(x), .TERM):
		return .VAR(x)
	case let (.Apply(.LAM, args), _):
		switch args.match1 {
		case let .E(.Binding(_, x, e)):
			return .LAM(x[0], e)
		}
	case let (.Apply(.AP, args), _):
		switch args.match2 {
		case let .E(.Binding(_, _, e1), .Binding(_, _, e2)):
			return .AP(e1, e2)
		}
	default:
		fatalError("Invalid")
	}
}

fileprivate func asType(_ t : STLCℯ) -> TypeView {
	switch t.infer {
	case let (.Variable(x), .TYPE):
		return .TVAR(x)
	case let (.Apply(.ARR, args), _):
		switch args.match2 {
		case let .E(.Binding(_, _, t1), .Binding(_, _, t2)):
			return .ARR(t1, t2)
		}
	default:
		fatalError("\(t) is not a type!")
	}
}

fileprivate func eval(_ c : STLCTerm) -> STLCTerm {
	var ct = OperationalSemantics.Closure(c, [:])
	while case let .STEP(c1) = ct.step {
		ct = c1
	}
	return ct.term
}

fileprivate indirect enum OperationalSemantics {
	struct Closure {
		let term : STLCTerm
		let env  : Dictionary<STLCTerm.ABTVariable, Closure>
		
		init(_ term : STLCTerm, _ env : Dictionary<STLCTerm.ABTVariable, Closure>) {
			self.term = term
			self.env = env
		}
		
		func withArgument(_ idx : Int, _ f : (STLCTerm.ABTView) -> OperationalSemantics) -> OperationalSemantics {
			let (theta, es) = self.viewBinding
			guard case let .Binding(us, xs, n) = es[idx] else {
				fatalError("Wat?")
			}
			
			switch Closure(n, self.env).step {
			case .VAL:
				let v = n.toView
				switch v {
				case .Apply(_, _):
					return f(v)
				default:
					fatalError("Expected operator")
				}
			case let .STEP(c2):
				var a2 = es
				a2[idx] = .Binding(us, xs, c2.term)
				return .STEP(Closure(STLCTerm(checking: .Apply(theta, a2), self.term.sort), c2.env))
			}
		}
		
		var viewBinding : (STLCOperator, [STLCTerm.ABTBView]) {
			switch self.term.toView {
			case let .Apply(theta, es):
				return (theta, es)
			default:
				fatalError("Expected operator")
			}
		}
		
		var subterms : [STLCTerm.ABTBView] {
			return self.viewBinding.1
		}
		
		var step : OperationalSemantics {
			switch self.term.toView {
			case let .Variable(x):
				return .STEP(self.env[x]!)
			case let .Apply(theta, _):
				switch theta {
				case let .VAR(x):
					return .STEP(self.env[x]!)
				case .AP:
					return withArgument(0) { arg in
						switch arg {
						case let .Apply(.LAM, aa):
							guard case let .Binding(_, xs, mx) = aa.first! else {
								fatalError("Wat?")
							}
							guard case let .Binding(_, _, n) = self.subterms[1] else {
								fatalError("Wat?")
							}
							return .STEP(Closure(mx, self.env.insert(xs.first!, v: Closure(n, self.env))))
						default:
							fatalError("")
						}
					}
				default:
					return .VAL
				}
			case .MetaApply(_, _, _):
				fatalError("Unexpected Meta")
			}
		}
	}
	case VAL // Canonical forms
	case STEP(Closure) // One step of evaluation
}

	
fileprivate func infer(_ env : (Δ, Γ), _ e : STLCℯ) -> Optional<STLCτ> {
	switch asExpression(e) {
	case let .VAR(x):
		// Query the environment
		return env.1[x]
	case let .AP(r, e):
		// Otherwise...
		return infer(env, r).flatMap { t in
			switch asType(t) {
			// try to pull an arrow out
			case let .ARR(t1, t2):
				return check(env, e, t1) ? t2 : nil
			default:
				return nil
			}
		}
	case let .LAM(x, body):
		return env.1[x].flatMap { t in
			return infer((env.0.insert(x, v: ()), env.1.insert(x, v: t)), body).flatMap { u in
				return .some(TypeView.ARR(t, u).intoSTLC)
			}
		}
	}
}

fileprivate func check(_ env : (Δ, Γ), _ e : STLCℯ, _ t : STLCτ) -> Bool {
	let v = asExpression(e)
	switch (v, asType(t)) {
	case let (.LAM(x, e), .ARR(t1, t2)):
		return check((env.0, env.1.insert(x, v: t1)), e, t2)
	default:
		return false
	}
}

import XCTest

class STLCTests : XCTestCase {
	func testThings() {
		let expr : STLCℯ.ABTView = .Apply(.AP, [
			.Binding([], [], STLCℯ(applying: .LAM,  to: [
				.Binding([], ["x"], STLCℯ(applying: .VAR("x"),  to: []))
			])),
			.Binding([], [], STLCℯ(applying: .LAM,  to: [
				.Binding([], ["y"], STLCℯ(applying: .VAR("y"),  to: []))
			])),
		])
		let ev = eval(STLCℯ(checking: expr, .TERM))
		print(infer(([:], [:]), ev))
		print(ev)
	}
}


