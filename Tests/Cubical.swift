//
//  Cubical.swift
//  Valence
//
//  Created by Robert Widmann on 8/7/16.
//  Copyright © 2016 TypeLift. All rights reserved.
//

import Valence

// MARK: Binding Structure

fileprivate enum CubicalSort : Sort {
	case EXP
	case DIM
	
	var description: String {
		switch self {
		case .EXP:
			return "exp"
		case .DIM:
			return "dim"
		}
	}

	static func == (l : CubicalSort, r : CubicalSort) -> Bool {
		switch (l, r) {
		case (.EXP, .EXP):
			return true
		case (.DIM, .DIM):
			return true
		default:
			return false
		}
	}
}

fileprivate enum CubicalOperator : Operator {
	typealias OperatorSort = CubicalSort

	case FUN
	case PROD
	// ID
	case BOOL
	case NOT
	case S1
	case LAM
	case APP
	case PAIR
	case FST
	case SND
	case TRUE
	case FALSE
	case IF
	case NOT_EL
	case BASE
	case LOOP
	case S1_ELIM
	case COE
	case HCOM
	case D0
	case D1
	case DREF(Dimension) // <x>M
	
	var arity : Arity<CubicalSort> {
		switch self {
		case .FUN:
			return Arity([ 
				Valence([], [], .EXP), 
				Valence([], [], .EXP) 
			], .EXP)
		case .PROD:
			return Arity([ 
				Valence([], [], .EXP), 
				Valence([], [], .EXP) 
			], .EXP)
		case .BOOL:
			return Arity([], .EXP)
		case .NOT:
			return Arity([ 
				Valence([], [], .EXP) 
			], .EXP)
		case .S1:
			return Arity([], .EXP)
		case .LAM:
			return Arity([ 
				Valence([], [ .EXP ], .EXP) 
			], .EXP)
		case .APP:
			return Arity([ 
				Valence([], [], .EXP), 
				Valence([], [], .EXP) 
			], .EXP)
		case .PAIR:
			return Arity([ 
				Valence([], [], .EXP), 
				Valence([], [], .EXP) 
			], .EXP)
		case .FST:
			return Arity([
				Valence([], [], .EXP) 
			], .EXP)
		case .SND:
			return Arity([ 
				Valence([], [], .EXP) 
			], .EXP)
		case .TRUE:
			return Arity([], .EXP)
		case .FALSE:
			return Arity([], .EXP)
		case .IF:
			return Arity([
				Valence([], [], .EXP),
				Valence([], [], .EXP),
				Valence([], [], .EXP),
				Valence([], [], .EXP),
			], .EXP)
		case .NOT_EL:
			return Arity([
				Valence([], [], .DIM),
				Valence([], [], .EXP),
			], .EXP)
		case .BASE:
			return Arity([], .EXP)
		case .LOOP:
			return Arity([
				Valence([], [], .DIM),
			], .EXP)
		case .S1_ELIM:
			return Arity([
				Valence([], [], .EXP),
				Valence([], [], .EXP),
				Valence([], [], .EXP),
				Valence([.DIM], [], .EXP),
			], .EXP)
		case .COE:
			return Arity([
				Valence([], [], .DIM),
				Valence([], [], .DIM),
				Valence([], [ .DIM ], .EXP),
				Valence([], [], .EXP),
			], .EXP)
		case .HCOM:
			return Arity([
				Valence([], [], .DIM),
				Valence([], [], .EXP),
				Valence([], [], .DIM),
				Valence([], [], .DIM),
				Valence([], [], .EXP),
				Valence([ .DIM ], [], .EXP),
				Valence([ .DIM ], [], .EXP),
			], .EXP)
		case .D0:
			return Arity([], .DIM)
		case .D1:
			return Arity([], .DIM)
		case .DREF(_):
			return Arity([], .DIM)
		}
	}
	
	var description: String {
		switch self {
		case .FUN:
			return "fun"
		case .PROD:
			return "prod"
		// ID
		case .BOOL:
			return "bool"
		case .NOT:
			return "not"
		case .S1:
			return "s1"
		case .LAM:
			return "lam"
		case .APP:
			return "app"
		case .PAIR:
			return "pair"
		case .FST:
			return "fst"
		case .SND:
			return "snd"
		case .TRUE:
			return "true"
		case .FALSE:
			return "false"
		case .IF:
			return "if"
		case .NOT_EL:
			return "not"
		case .BASE:
			return "base"
		case .LOOP:
			return "loop"
		case .S1_ELIM:
			return "s1_elim"
		case .COE:
			return "coe"
		case .HCOM:
			return "hcom"
		case .D0:
			return "d0"
		case .D1:
			return "d1"
		case let .DREF(d):
			return "dref(\(d))"
		}
	}

	static func == (l : CubicalOperator, r : CubicalOperator) -> Bool {
		switch (l, r) {
		case (.FUN, .FUN):
			return true
		case (.PROD, .PROD):
			return true
		case (.BOOL, .BOOL):
			return true
		case (.NOT, .NOT):
			return true
		case (.S1, .S1):
			return true
		case (.LAM, .LAM):
			return true
		case (.APP, .APP):
			return true
		case (.PAIR, .PAIR):
			return true
		case (.FST, .FST):
			return true
		case (.SND, .SND):
			return true
		case (.TRUE, .TRUE):
			return true
		case (.FALSE, .FALSE):
			return true
		case (.IF, .IF):
			return true
		case (.NOT_EL, .NOT_EL):
			return true
		case (.BASE, .BASE):
			return true
		case (.LOOP, .LOOP):
			return true
		case (.S1_ELIM, .S1_ELIM):
			return true
		case (.COE, .COE):
			return true
		case (.HCOM, .HCOM):
			return true
		case (.D0, .D0):
			return true
		case (.D1, .D1):
			return true
		case let (.DREF(i), .DREF(j)):
			return i == j
		default:
			return false
		}
	}
}

// MARK: Cubical Syntax

fileprivate typealias CubicalTerm = AbstractBindingTree<Named, Named, Named, CubicalOperator, ()>

fileprivate protocol CubicalSyntax {
	init?(_ t : CubicalTerm)
	var asTerm : CubicalTerm { get }
}

fileprivate struct If : CubicalSyntax {
	let motive : CubicalTerm
	let scrutinee : CubicalTerm
	let trueCase : CubicalTerm
	let falseCase : CubicalTerm
	
	init(_ motive : CubicalTerm, _ scrutinee : CubicalTerm, _ trueCase : CubicalTerm, _ falseCase : CubicalTerm) {
		self.motive = motive
		self.scrutinee = scrutinee
		self.trueCase = trueCase
		self.falseCase = falseCase
	}
	
	init?(_ t : CubicalTerm) {
		switch t.toView {
		case let .Apply(.IF, args):
			assert(args.count == 4)
			self = If(
				args[0].body,
				args[1].body,
				args[2].body,
				args[3].body
			)
		default:
			return nil
		}
	}
	
	var asTerm: CubicalTerm {
		return CubicalTerm(checking: .Apply(.IF, [
			.Binding([], [], motive),
			.Binding([], [], scrutinee),
			.Binding([], [], trueCase),
			.Binding([], [], falseCase),
		]), .EXP)
	}
	
	func withScrutinee(_ s : CubicalTerm) -> If {
		return If(
			self.motive,
			s,
			self.trueCase,
			self.falseCase
		)
	}
}

// Kan Conditions

fileprivate enum Dimension : CubicalSyntax, Equatable {
	enum Interval {
		case I0
		case I1
	}
	
	case DTerm(Interval)
	indirect case Name(Dimension)
	
	init?(_ t : CubicalTerm) {
		switch t.toView {
		case let .Apply(.D0, args) where args.isEmpty:
			self = .DTerm(.I0)
		case let .Apply(.D1, args) where args.isEmpty:
			self = .DTerm(.I1)
		case let .Apply(.DREF(u), args) where args.isEmpty:
			self = .Name(u)
		default:
			return nil
		}
	}
	
	var asTerm: CubicalTerm {
		switch self {
		case .DTerm(.I0):
			return CubicalTerm(checking: .Apply(.D0, []), .DIM)
		case .DTerm(.I1):
			return CubicalTerm(checking: .Apply(.D1, []), .DIM)
		case let .Name(u):
			return CubicalTerm(checking: .Apply(.DREF(u), []), .DIM)
		}
	}

	static func == (l : Dimension, r : Dimension) -> Bool {
		switch (l, r) {
		case let (.DTerm(i), .DTerm(j)):
			return i == j
		case let (.Name(x), .Name(y)):
			return x == y
		default:
			return false
		}
	}
}

/// Coercions take an x-line between types A and elements M of A<r/x> to an
/// element A<r'/x>.  This is a generalization of the transport operation
/// described in the HoTT Book.
fileprivate struct Coe : CubicalSyntax {
	let start : Dimension
	let end : Dimension
	let line : (Named, CubicalTerm)
	
	init(_ start : Dimension, _ end : Dimension, _ line : (Named, CubicalTerm)) {
		self.start = start
		self.end = end
		self.line = line
	}
	
	init?(_ t : CubicalTerm) {
		switch t.toView {
		case let .Apply(.COE, args) where args.count == 3:
			self = Coe(
				Dimension(args[0].body)!,
				Dimension(args[1].body)!,
				(args[2].symbols[0], args[2].body) 
			)
		default:
			return nil
		}
	}
	
	var asTerm : CubicalTerm {
		let r1 = self.start.asTerm
		let r2 = self.end.asTerm
		let (u, a) = self.line
		return CubicalTerm(checking: .Apply(.COE, [
			.Binding([], [], r1),
			.Binding([], [], r2),
			.Binding([u], [], a),
		]), .EXP)
	}
}

/// Homogeneous Kan Composition states that open boxes in any type have lids.
/// This operation endows types with a higher groupoid structure by ensuring
/// that lines can be composed, reversed, and so forth.
fileprivate struct HCom : CubicalSyntax {
	struct Tube {
		let start : (Named, CubicalTerm)
		let end   : (Named, CubicalTerm)
		
		init(_ start : (Named, CubicalTerm), _ end :   (Named, CubicalTerm)) {
			self.start = start
			self.end = end
		}
	}
	
	let extent : Dimension
	let type : CubicalTerm
	let start : Dimension
	let end : Dimension
	let cap : CubicalTerm
	let tube : Tube
	
	init(_ extent : Dimension, _ type : CubicalTerm, _ start : Dimension, _ end : Dimension, _ cap : CubicalTerm, _ tube : Tube) {
		self.extent = extent
		self.type = type
		self.start = start
		self.end = end
		self.cap = cap
		self.tube = tube
	}
	
	init?(_ t : CubicalTerm) {
		switch t.toView {
		case let .Apply(.HCOM, args) where args.count == 7:
			self = HCom(
				Dimension(args[0].body)!,
				args[1].body,
				Dimension(args[2].body)!,
				Dimension(args[3].body)!,
				args[4].body,
				Tube((args[5].variables[0], args[5].body),
					 (args[6].variables[0], args[6].body))
			)
		default:
			return nil
		}
	}
	
	var asTerm : CubicalTerm {
		let (y0, n0) = tube.start
		let (y1, n1) = tube.end
		return CubicalTerm(checking: .Apply(.HCOM, [
			.Binding([], [], self.extent.asTerm),
			.Binding([], [], self.type),
			.Binding([], [], self.start.asTerm),
			.Binding([], [], self.end.asTerm),
			.Binding([], [], self.cap),
			.Binding([y0], [], n0),
			.Binding([y1], [], n1),
		]), .EXP)
	}
	
	func withType(_ t : CubicalTerm) -> HCom {
		return HCom(
			self.extent,
			t,
			self.start,
			self.end,
			self.cap,
			self.tube
		)
	}
	
	func map(_ f : (CubicalTerm) -> CubicalTerm) -> HCom {
		return HCom(
			self.extent,
			self.type,
			self.start,
			self.end,
			f(self.cap),
			Tube((self.tube.start.0, f(self.tube.start.1)),
				 (self.tube.end.0, f(self.tube.end.1)))
		)
	}
}

// Operational Semantics

fileprivate func eval(_ c : CubicalTerm) -> CubicalTerm {
	var ct = OperationalSemantics.Closure(c, [:])
	while case let .STEP(c1) = ct.step {
		ct = c1
	}
	return ct.term
}

fileprivate indirect enum OperationalSemantics {
	struct Closure {
		let term : CubicalTerm
		let env  : Dictionary<CubicalTerm.ABTVariable, Closure>
		
		init(_ term : CubicalTerm, _ env : Dictionary<CubicalTerm.ABTVariable, Closure>) {
			self.term = term
			self.env = env
		}
		
		func withArgument(_ idx : Int, _ f : (CubicalTerm.ABTView) -> OperationalSemantics) -> OperationalSemantics {
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
				return .STEP(Closure(CubicalTerm(checking: .Apply(theta, a2), self.term.sort), c2.env))
			}
		}
		
		var viewBinding : (CubicalOperator, [CubicalTerm.ABTBView]) {
			switch self.term.toView {
			case let .Apply(theta, es):
				return (theta, es)
			default:
				fatalError("Expected operator")
			}
		}

		// TODO: This operational semantics only implements the boolean fragment
		// of the full cubical TT described here:
		// ~( http://www.cs.cmu.edu/~rwh/papers/chitt/draft.pdf )
		var step : OperationalSemantics {
			switch self.term.toView {
			case let .Variable(x):
				return .STEP(self.env[x]!)
			case .MetaApply(_, _, _):
				fatalError("Unexpected Meta")
			case let .Apply(theta, _):
				switch theta {
				// Booleans
				case .BOOL:
				// ------------
					return .VAL
				case .TRUE:
				// ------------
					return .VAL
				case .FALSE:
				// ------------
					return .VAL
				case .NOT:
					guard case let .some(.Binding(_, _, e)) = self.viewBinding.1.first else {
						fatalError("")
					}
					switch Dimension(e)! {
					case .DTerm(_):
						return .STEP(Closure(CubicalTerm(checking: .Apply(.BOOL, []), .EXP), self.env))
					case .Name(_):
						return .VAL
					}
					
				// Dependent functions
				case .FUN:
					return .VAL
				case .PROD:
					return .VAL
				case .LAM:
					return .VAL
				case .APP:
					return withArgument(0) { arg in
						switch arg {
						case let .Apply(.LAM, aa):
							guard case let .Binding(_, xs, mx) = aa.first! else {
								fatalError("Wat?")
							}
							guard case let .Binding(_, _, n) = self.viewBinding.1[1] else {
								fatalError("Wat?")
							}
							return .STEP(Closure(mx, self.env.insert(xs.first!, v: Closure(n, self.env))))
						default:
							fatalError("")
						}
					}
				case .PAIR:
					return .VAL
				case .FST:
					return withArgument(0) { arg in
						switch arg {
						case let .Apply(.PAIR, aa):
							guard case let .Binding(_, _, m1) = aa[0] else {
								fatalError("Wat?")
							}
							return .STEP(Closure(m1, self.env))
						default:
							fatalError("")
						}
					} 
				case .SND:
					return withArgument(0) { arg in
						switch arg {
						case let .Apply(.PAIR, aa):
							guard case let .Binding(_, _, m2) = aa[1] else {
								fatalError("Wat?")
							}
							return .STEP(Closure(m2, self.env))
						default:
							fatalError("")
						}
					} 
				case .IF:
					// Inspect the scrutinee
					return withArgument(1) { arg in
						guard let val = If(self.term) else {
							fatalError("")
						}
						switch arg {
						// ---------------------------------------------
						// if a.A (false; T, F) |-------------------> F
						case .Apply(.FALSE, _):
							return .STEP(Closure(val.falseCase, self.env))
						// ---------------------------------------------
						// if a.A (true; T, F) |--------------------> T
						case .Apply(.TRUE, _):
							return .STEP(Closure(val.trueCase, self.env))
						// r != r', H = hcom (r ~> z, M; y.N)
						// ---------------------------------------------
						// if a.A (hcom (r ~> z, M; y.N); T, F) 
						//		-> hcom(r ~> r', coe(if a.A (; T, F)); y.coe(if(N; T, F)))				
						case .Apply(.HCOM, _):
							guard let hcom = HCom(self.term) else {
								fatalError("")
							}
							
							return .STEP(Closure(hcom.withType(val.motive).map({ m in
								return val.withScrutinee(m).asTerm
							}).asTerm, self.env))
						default:
							fatalError("")
						}
					}
				case .COE:
					// Dig out the body of the coercion
					return withArgument(2) { arg in
						guard Coe(self.term) != nil else {
							fatalError("")
						}

						switch arg {
						// -------------------------------------------
						//               coe(M) |-----> M
						case let .Apply(.BOOL, xs) where xs.isEmpty:
							return .STEP(Closure(self.term, self.env))
						default:
							fatalError("")
						}
					}
				default:
					fatalError("unimplemented")
				}
			}
		}
	}
	case VAL // Canonical forms
	case STEP(Closure) // One step of evaluation
}

import XCTest

class CubicalTests : XCTestCase {
	func testThings() {
		let expr : CubicalTerm.ABTView = .Apply(.IF, [
			.Binding([], [], CubicalTerm(applying: .BOOL,  to:  [])),
			.Binding([], [], CubicalTerm(applying: .TRUE,  to:  [])),
			.Binding([], [], CubicalTerm(applying: .TRUE,  to:  [])),
			.Binding([], [], CubicalTerm(applying: .FALSE, to:  [])),
		])
		let ev : CubicalTerm = eval(CubicalTerm(checking: expr, .EXP))
		print(ev)
		
		let expr2 : CubicalTerm.ABTView = .Apply(.IF, [
			.Binding([], [], CubicalTerm(applying: .BOOL,  to: [])),
			.Binding([], [], CubicalTerm(applying: .FALSE, to: [])),
			.Binding([], [], CubicalTerm(applying: .TRUE,  to: [])),
			.Binding([], [], CubicalTerm(applying: .FALSE, to: [])),
		])
		let ev2 : CubicalTerm = eval(CubicalTerm(checking: expr2, .EXP))
		print(ev2)
	}
}
