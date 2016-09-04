//
//  SystemF.swift
//  Valence
//
//  Created by Robert Widmann on 8/16/16.
//  Copyright © 2016 TypeLift. All rights reserved.
//

import Valence

fileprivate enum SystemFSort : Sort {
	case TYPE
	case EXPR
	
	static func ==(l : SystemFSort, r : SystemFSort) -> Bool {
		switch (l, r) {
		case (.TYPE, .TYPE):
			return true
		case (.EXPR, .EXPR):
			return true
		default:
			return false
		} 
	}
	
	var description : String {
		switch self {
		case .TYPE: return "type"
		case .EXPR: return "expr"
		}
	}
}

fileprivate enum SystemFOperator : Operator {
	typealias OperatorSort = SystemFSort

	// λ (x : τ) . e
	case LAM
	// e1(e2)
	case AP
	// Λ(t . e)
	case TYLAM
	// e[τ]
	case TYAPP

	// τ1 -> τ2
	case ARR
	// ∀ (t : τ)
	case ALL
	// (t ∈ A)
	case ANN
	
	var arity : Arity<SystemFSort> {
		switch self {
		case .LAM: // λ
			return Arity([ // (x : τ) . e
				Valence([], [.EXPR], .EXPR),
			], .EXPR)
		case .AP: 
			return Arity([
				Valence([], [], .EXPR), // e1
				Valence([], [], .EXPR), // (e2)
			], .EXPR)
		case .TYLAM: // Λ
			return Arity([ // (t .    e)
				Valence([], [.TYPE], .EXPR),
			], .EXPR)
		case .TYAPP:
			return Arity([
				Valence([], [], .EXPR), // e
				Valence([], [], .TYPE), // [τ]
			], .EXPR)
		case .ARR:
			return Arity([
				Valence([], [], .TYPE), // τ1
				Valence([], [], .TYPE), // τ2
			], .TYPE)
		case .ALL: // ∀
			return Arity([ // (t :   τ)
				Valence([], [.TYPE], .TYPE),
			], .TYPE)
		case .ANN:
			return Arity([
				Valence([], [], .EXPR), // t ∈
				Valence([], [], .TYPE), // A
			], .EXPR)
		}
	}
	
	var description : String {
		switch self {
		case .LAM: return "lam"
		case .AP: return "ap"
		case .TYLAM: return "tylam"
		case .TYAPP: return "tyapp"
		case .ARR: return "arr"
		case .ALL: return "forall"
		case .ANN: return "ann"
		}
	}
}

fileprivate typealias SystemFTerm = AbstractBindingTree<Named, Named, Named, SystemFOperator, ()>
fileprivate typealias Fτ = SystemFTerm
fileprivate typealias Fℯ = SystemFTerm

fileprivate typealias Δ = Dictionary<Named, ()>
fileprivate typealias Γ = Dictionary<Named, Fτ>


fileprivate enum ValueView<T, E> {
	case TLAM(Named, E)
	case LAM(Named, E)
}

fileprivate enum NeutralView<T, E> {
	case VAR(Named)
	case TAPP(E, T)
	case AP(E, E)
}

fileprivate enum ExprView<T, E> {
	case ANN(E, T)
	case NEU(NeutralView<T, E>)
	case VAL(ValueView<T, E>)
}

fileprivate enum TypeView {
	case TVAR(Named)
	case ARR(Fτ, Fτ)
	case ALL(Named, Fτ)
	
	var intoSystemF : Fτ {
		switch self {
		case let .TVAR(x):
			return Fτ(checking: .Variable(x), .TYPE)
		case let .ARR(t1, t2):
			return Fτ(applying: .ARR, to: [
				.Binding([], [], t1),
				.Binding([], [], t2),
			])
		case let .ALL(x, t):
			return Fτ(applying: .ALL, to: [
				.Binding([], [x], t)
			])
		}
	}
}

fileprivate func asExpression(_ e : Fℯ) -> ExprView<Fτ, Fℯ> {
	switch e.infer {
	case let (.Variable(x), .EXPR):
		return .NEU(.VAR(x))
	case let (.Apply(.TYLAM, args), _):
		switch args.match1 {
		case let .E(.Binding(_, x, e)):
			return .VAL(.TLAM(x[0], e))
		}
	case let (.Apply(.TYAPP, args), _):
		switch args.match2 {
		case let .E(.Binding(_, _, e), .Binding(_, _, t)):
			return .NEU(.TAPP(e, t))
		}
	case let (.Apply(.LAM, args), _):
		switch args.match1 {
		case let .E(.Binding(_, x, e)):
			return .VAL(.LAM(x[0], e))
		}
	case let (.Apply(.AP, args), _):
		switch args.match2 {
		case let .E(.Binding(_, _, e1), .Binding(_, _, e2)):
			return .NEU(.AP(e1, e2))
		}
	case let (.Apply(.ANN, args), _):
		switch args.match2 {
		case let .E(.Binding(_, _, e), .Binding(_, _, t)):
			return .ANN(e, t)
		}
	default:
		fatalError("Invalid")
	}
}

fileprivate func asType(_ t : Fℯ) -> TypeView {
	switch t.infer {
	case let (.Variable(x), .TYPE):
		return .TVAR(x)
	case let (.Apply(.ARR, args), _):
		switch args.match2 {
		case let .E(.Binding(_, _, t1), .Binding(_, _, t2)):
			return .ARR(t1, t2)
		}
	case let (.Apply(.ALL, args), _):
		switch args.match1 {
		case let .E(.Binding(_, x, t)):
			return .ALL(x[0], t)
		}
	default:
		fatalError("\(t) is not a type!")
	}
}

// Checking is pretty easy
fileprivate func checkType(_ gamma : Δ, _ t : Fτ) -> Bool {
	switch asType(t) {
	//
	// ---------
	// Γ ⊢ x : A
	case let .TVAR(x):
		return gamma[x] != nil
	//    Γ; x : A ⊢ t : B
	// -----------------------
	// Γ ⊢ λ x : A . t : A → B
	case let .ARR(t1, t2):
		return checkType(gamma, t1) && checkType(gamma, t2)
	//   Γ ⊢ t : ∀ α . B
	// -------------------
	// Γ ⊢ (t A) : B {α := A}
	case let .ALL(x, t):
		return checkType(gamma.insert(x, v: ()), t)
	}
}

fileprivate func check(_ env : (Δ, Γ), _ e : Fℯ, _ t : Fτ) -> Bool {
	switch asExpression(e) {
	case let .VAL(v):
		return checkVal(env, v, t)
	case let .NEU(r):
		switch inferNeu(env, r) {
		case let .some(t2):
			return t == t2
		default:
			return false
		}
	case let .ANN(e, t2):
		// Syntactic equality suffices.
		return (t == t2) && check(env, e, t)
	}
}

fileprivate func checkVal(_ env : (Δ, Γ), _ v : ValueView<Fτ, Fℯ>, _ t : Fτ) -> Bool {
	switch (v, asType(t)) {
	case let (.LAM(x, e), .ARR(t1, t2)):
		return check((env.0, env.1.insert(x, v: t1)), e, t2)
	case let (.TLAM(z, e), .ALL(x, t)):
		return check((env.0.insert(x, v: ()), env.1), e, t.substitute(TypeView.TVAR(z).intoSystemF, for: x))
	default:
		return false
	}
}

// Naive inference.
fileprivate func inferExp(_ env : (Δ, Γ), _ e : Fℯ) -> Optional<Fτ> {
	switch asExpression(e) {
	case let .NEU(r):
		// Try a bit of substitution.
		return inferNeu(env, r)
	case let .ANN(_, t):
		// Oh look, a type.
		return .some(t)
	default:
		// Inference is undecidable in general.
		return nil
	}
}

fileprivate func inferNeu(_ env : (Δ, Γ), _ r : NeutralView<Fτ, Fℯ>) -> Optional<Fτ> {
	switch r {
	case let .VAR(x):
		// Query the environment
		return env.1[x]
	case let .AP(r, e):
		// Otherwise...
		return inferExp(env, r).flatMap { t in
			switch asType(t) {
			// try to pull an arrow out
			case let .ARR(t1, t2):
				return check(env, e, t1) ? t2 : nil
			default:
				return nil
			}
		}
	case let .TAPP(r, t):
		// Try to pull out a forall.
		if checkType(env.0, t) {
			return inferExp(env, r).flatMap { t2 in
				switch asType(t2) {
				case let .ALL(x, t3):
					// Perform a bit of computation.
					return .some(t3.substitute(t, for: x))
				default:
					// What the hell?
					return nil
				}
			}
		}
		return nil
	}
}

